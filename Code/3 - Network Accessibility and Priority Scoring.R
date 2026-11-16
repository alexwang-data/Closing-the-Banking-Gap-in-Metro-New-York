## Part 3: Network Accessibility and Priority Scoring___________________________


# create two sets of destination points
main_pts <- bind_rows(banks_sf %>% select(geometry),
                      cu_sf %>% select(geometry))                               # banks + credit unions

afs_pts <- afs_sf %>% select(geometry)                                          # pawns + check cashers


# erase large water bodies (rivers, bays, harbor)
# area_threshold = 0.75, erase only the largest 25% of water features
tracts_land <- erase_water(spatial_df, area_threshold = 0.75)

# remove slivers left by erase_water (# 10,000 m² = 1 hectare)
clean_geom <- tracts_land %>%
  select(GEOID) %>%
  st_collection_extract("POLYGON") %>%                                          # keep polygon parts only
  st_cast("POLYGON", warn = FALSE) %>%                                          # splits a tract made of several pieces into one row per piece
  mutate(piece_area = as.numeric(st_area(.))) %>%                               # area of each piece (m2)
  group_by(GEOID) %>%
  filter(piece_area >= 10000 |                                                  # keep pieces of 1 hectare or more
           piece_area == max(piece_area)) %>%                                   # or the tract's largest pieces
  summarise(do_union = TRUE) %>%                                                # merge remaining pieces back into one shape per tract
  ungroup()


# reattach the cleaned shapes to the tract data
tracts_land <- tracts_land %>%         
  st_drop_geometry() %>%                                                        # remove the old, sliver-filled shapes
  left_join(clean_geom, by = "GEOID") %>%                                       # add the cleaned shapes
  st_as_sf()                                                                    # mark it as spatial again


nrow(tracts_land) == nrow(spatial_df)                                           # TRUE = no tracts lost



# 2020 Census tract centers of population
# the average location of each tract's residents
# so distance are measured from where people actually live

# read csv
cenpop <- read.csv(
  "https://www2.census.gov/geo/docs/reference/cenpop2020/tract/CenPop2020_Mean_TR.txt",
  colClasses = c(STATEFP = "character", COUNTYFP = "character", TRACTCE = "character"),
  fileEncoding = "UTF-8-BOM"
)

# census tract center of population
tract_pts <- cenpop %>%
  mutate(GEOID = paste0(STATEFP, COUNTYFP, TRACTCE)) %>%                        # build the 11-digit tract ID
  filter(GEOID %in% tracts_land$GEOID) %>%                                      # keep study tracts only
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4269) %>%                 # Census uses NAD83 = 4269, x = long, y = lat
  st_transform(st_crs(tracts_land)) %>%                                         # match projection
  select(GEOID)


# center of population and tract: match points with GEOID (tract 1 with point 1, etc)
tract_pts <- tract_pts[match(tracts_land$GEOID, tract_pts$GEOID), ]

sum(is.na(tract_pts$GEOID))                                                     # 0 = every tract has a population center
all(tract_pts$GEOID == tracts_land$GEOID)                                       # TRUE = rows line up



# ACS: share of households with no vehicle
vehicle <- get_acs(geography = "tract",
                   state = c("NY", "NJ", "PA"),
                   variables = c(total = "B08201_001", no_vehicle = "B08201_002"),
                   year = 2023,
                   survey = "acs5",
                   output = "wide") %>%
  transmute(GEOID,
            households = totalE,                                                # total households
            pct_no_vehicle = if_else(totalE > 0,                                # avoid dividing by zero
                                     no_vehicleE / totalE * 100,    
                                     NA_real_))                                 # tracts with no households = NA


# I did some research into urban, suburban, and rural tracts, and NY CBSA should be urban and suburban only.
# fed dashboard threshold: 2 miles for urban, 5 miles for suburban, 10 miles for rural.
# a tract is rural if not nested inside a MSA.

# if use census urban area delineation, 4637 out of 4761 tracts will be classified as urban, which may not be accurate to determine how people travel, walking or driving.
# I will approach urban/suburban classification with car-free households.

# if a tract has 25% or more car free households, then it's classified as urban and use walking network distance.
# the study is concerned with the transportation methods of walking and driving. Thus, an appropriate approach.



# tract classification: urban (walk) vs suburban (drive)
site_df <- tracts_land %>%
  left_join(vehicle, by = "GEOID") %>%                                          # add household % with no vehicle
  left_join(acs_data %>% st_drop_geometry() %>%
              select(GEOID, med_income), by = "GEOID") %>%                      # add median household income
  mutate(
    dense     = coalesce(pct_no_vehicle >= 25, FALSE),                          # urban = at least 25% of household have no car, tracts with missing vehicle data become suburban
    area_type = if_else(dense, "urban", "suburban"),                            # relabel
  )


# checks
table(site_df$area_type)                                                        # Urban: 2060, Suburban: 2701



# nearest bank or credit union (straight-line)
# for each tract's population center, find the closest branch
nearest <- st_nearest_feature(tract_pts, main_pts)


# straight-line distance and count
site_df <- site_df %>%
  mutate(
    # straight-line distance to the nearest branch (m)
    # used as a fallback when network fails
    dist_bank_m = as.numeric(st_distance(tract_pts, main_pts[nearest, ], by_element = TRUE)),
    
    # urban fallback counts: branches and afs within 1.25 km straight-line
    # used only if the walking network fails
    banks_1km   = lengths(st_is_within_distance(tract_pts, main_pts, dist = 1250)),
    afs_1km     = lengths(st_is_within_distance(tract_pts, afs_pts,  dist = 1250)),
    
    # suburban counts: branches and afs inside the tract boundary
    banks_far   = n_main,
    afs_far     = n_afs
  )


# check counts
table(site_df$tier)



# filter vehicle free pct to the study tracts 
x <- vehicle %>%
  filter(GEOID %in% site_df$GEOID) %>%
  pull(pct_no_vehicle) %>%
  na.omit

length(x)                                                                       # 4761 tracts


# natural jenks: 2 classification

# split the values into groups so that values within each group are as similar as possible and the groups are as different as possible
classIntervals(x, n = 2, style = "fisher")$brks                                 # 37%


# histogram of car-free households

# elbow at around 20%, where the car-dependent cluster ends
# 25% is about 3x the national rate, and the start of the car-light right tail
hist(x, breaks = 60, main = "Share of households without a vehicle", xlab = "%")
abline(v = 25, col = "red", lwd = 2)




# Urban Walking Network
#OSM data is stored in projection 4326 (longitude/latitude)

# urban tract points and the area the network must cover
dense_ids <- site_df %>% st_drop_geometry() %>% filter(dense) %>% pull(GEOID)   # IDs of urban tracts
dense_pts <- tract_pts %>% filter(GEOID %in% dense_ids)                         # their population centers

# buffer 1.75 km around all urban points: cover any bank reachable within a 15-minute walk
dense_area <- dense_pts %>% st_union() %>% st_buffer(1750)          
dense_bbox <- st_as_sfc(st_bbox(st_transform(dense_area, 4326)))                # rectangle in lat/long, for clipping OSM


# download / read the OpenStreetMap street network, manual download using links if timed out
# https://download.geofabrik.de/north-america/us/new-york-latest.osm.pbf
# https://download.geofabrik.de/north-america/us/new-jersey-latest.osm.pbf

get_streets <- function(region) {
  oe_get(region,
         layer = "lines",                                                       # roads and paths are stored as lines
         boundary = dense_bbox, boundary_type = "spat",                         # keep only the study rectangle
         query = "SELECT * FROM lines WHERE highway IS NOT NULL",               # any road or path
         extra_tags = c("foot", "access", "oneway"),                            # walking/driving tags
         quiet = FALSE)
}


# delete the broken / partial files
#dir <- oe_download_directory()
#old <- list.files(dir, pattern = "new-york|new-jersey", full.names = TRUE)
#old                                                                            # check what will be removed
#file.remove(old)


streets <- bind_rows(get_streets("us/new-york"), get_streets("us/new-jersey")) %>%
  select(osm_id, highway, foot, access) %>%                                     # tags: foot, access
  st_transform(st_crs(tracts_land))                                             # projection


# GEOID = 11 digits
# state = 2 digits
# county = 3 digits
# tract = 6 digits

# walking distances and times, one county at a time
counties <- unique(substr(dense_pts$GEOID, 1, 5))                               # take characters 1 through 5, county FIPS codes of urban tracts
results <- list()                                                               # one table per county


for (cf in counties) {
  message("County ", cf, " (", which(counties == cf), " of ", length(counties), ")")
  
  # this county's tract points and a 1.75 km search around them
  pts  <- dense_pts %>% filter(substr(GEOID, 1, 5) == cf)
  area <- pts %>% st_union() %>% st_buffer(1750)
  
  # build the walking network for this area
  net_c <- st_filter(streets, area)
  if (nrow(net_c) == 0) next                                                    # skip if no streets
  graph <- weight_streetnet(st_transform(net_c, 4326), wt_profile = "foot")     # walking rule and speeds
  
  
  graph <- graph[is.finite(graph$d_weighted) & graph$d_weighted > 0 &           # drop unwalkable segments
                   is.finite(graph$time_weighted) & graph$time_weighted > 0, ]  # and those with no valid time
  graph <- dodgr_components(graph)                                              # label connected pieces of the network
  comp_size <- table(graph$component)
  keep <- as.integer(names(comp_size)[comp_size >= 200])                        # keep pieces with 200+ segments
  graph <- graph[graph$component %in% keep, ]                                   # drop tiny isolated fragments
  
  # origins (tract points) and destinations (branches, AFS) within the area
  from_xy <- pts %>% st_transform(4326) %>% st_coordinates()
  banks_c <- main_pts %>% st_filter(area)
  afs_c   <- afs_pts  %>% st_filter(area)
  
  # if no destinations, a column of Inf = "none within reach"
  # walking distance to each branch (m)
  d_bank <- if (nrow(banks_c) > 0)
    dodgr_dists(graph, from = from_xy, to = st_coordinates(st_transform(banks_c, 4326))) else
      matrix(Inf, nrow(pts), 1)
  
  # walking time to each branch
  t_bank <- if (nrow(banks_c) > 0)
    dodgr_times(graph, from = from_xy, to = st_coordinates(st_transform(banks_c, 4326))) else
      matrix(Inf, nrow(pts), 1)
  
  # walking distance to each pawnshop or check casher
  d_afs  <- if (nrow(afs_c) > 0)
    dodgr_dists(graph, from = from_xy, to = st_coordinates(st_transform(afs_c, 4326))) else
      matrix(Inf, nrow(pts), 1)
  
  # summarize per tract
  results[[cf]] <- tibble(
    GEOID         = pts$GEOID,
    walk_bank_m   = suppressWarnings(apply(d_bank, 1, min, na.rm = TRUE)),      # Inf = none within reach
    walk_min      = suppressWarnings(apply(t_bank, 1, min, na.rm = TRUE)) / 60, # seconds → minutes
    walk_banks_1k = rowSums(d_bank <= 1250, na.rm = TRUE),                      # the number of banks and credit unions within a 1,250 walk
    walk_afs_1k   = rowSums(d_afs  <= 1250, na.rm = TRUE)                       # the number of pawnshops and check cashers within a 1,250 m walk
  )
}

walk_df <- bind_rows(results)                                                   # stack all counties
saveRDS(walk_df, "walk_df_final.rds")                                           # save so this slow step doesn't need rerunning






# Suburban Driving Network
#OSM data is stored in projection 4326 (longitude/latitude)

# suburban tract points and the area the network must cover
sub_ids  <- site_df %>% st_drop_geometry() %>% filter(!dense) %>% pull(GEOID)
sub_pts  <- tract_pts %>% filter(GEOID %in% sub_ids)

# buffer 15 km: cover any branch reachable within a 15-min drive
sub_area <- sub_pts %>% st_union() %>% st_buffer(15000)                         # meters
sub_bbox <- st_as_sfc(st_bbox(st_transform(sub_area, 4326)))                    # rectangle in lat/long, for clipping OSM

# road network (drivable roads only) 
get_roads <- function(region) {
  oe_get(region,
         layer = "lines",
         boundary = sub_bbox, boundary_type = "spat",
         # roads cars can use; excludes footpaths, cycleways, and steps
         query = "SELECT * FROM lines WHERE highway IN ('motorway','motorway_link','trunk','trunk_link',
                  'primary','primary_link','secondary','secondary_link','tertiary','tertiary_link',
                  'unclassified','residential','living_street','service')",
         extra_tags = c("foot", "oneway", "access"),                            # same tags as the walking step
         force_vectortranslate = TRUE,       
         quiet = FALSE)
}

roads <- bind_rows(get_roads("us/new-york"), get_roads("us/new-jersey")) %>%
  select(osm_id, highway, oneway, access) %>%                                   # select tags
  st_transform(st_crs(tracts_land))                                             # projection




# driving distances and times, one county at a time 
sub_counties <- unique(substr(sub_pts$GEOID, 1, 5))                             # county FIPS codes of suburban tracts
drive_results <- list()

for (cf in sub_counties) {
  message("Driving: county ", cf, " (", which(sub_counties == cf), " of ", length(sub_counties), ")")
  
  # this county's tract points and a 15 km search area around them
  pts  <- sub_pts %>% filter(substr(GEOID, 1, 5) == cf)
  area <- pts %>% st_union() %>% st_buffer(15000)
  
  # build the driving network
  net_c <- st_filter(roads, area)
  if (nrow(net_c) == 0) next
  
  # clean the network: drop invalid segments and tiny isolated fragments
  graph <- weight_streetnet(st_transform(net_c, 4326), wt_profile = "motorcar")
  graph <- graph[is.finite(graph$d_weighted) & graph$d_weighted > 0 &
                   is.finite(graph$time_weighted) & graph$time_weighted > 0, ]
  graph <- dodgr_components(graph)
  comp_size <- table(graph$component)
  graph <- graph[graph$component %in% as.integer(names(comp_size)[comp_size >= 200]), ]
  
  # origins and destinations (no AFS needed: suburban AFS are counted inside the tract)
  banks_c <- main_pts %>% st_filter(area)
  from_xy <- pts %>% st_transform(4326) %>% st_coordinates()
  
  # driving distance to each branch (m)
  d_bank <- if (nrow(banks_c) > 0)
    dodgr_dists(graph, from = from_xy, to = st_coordinates(st_transform(banks_c, 4326))) else
      matrix(Inf, nrow(pts), 1)
  
  # driving time to each branch (seconds)
  t_bank <- if (nrow(banks_c) > 0)
    dodgr_times(graph, from = from_xy, to = st_coordinates(st_transform(banks_c, 4326))) else
      matrix(Inf, nrow(pts), 1)
  
  drive_results[[cf]] <- tibble(
    GEOID         = pts$GEOID,
    drive_bank_m  = suppressWarnings(apply(d_bank, 1, min, na.rm = TRUE)),      # nearest branch (m); Inf = none within reach
    drive_min    = suppressWarnings(apply(t_bank, 1, min, na.rm = TRUE)) / 60   # nearest branch (minutes)
  )
}

drive_df <- bind_rows(drive_results)    # stack all counties
saveRDS(drive_df, "drive_df_final.rds") # save, so no need to rerun




# combine walking and driving results into final tiers
site_df <- site_df %>%
  select(-any_of(c("walk_bank_m", "walk_min", "walk_banks_1k", "walk_afs_1k",
                   "drive_bank_m", "drive_min",
                   "walk_fail", "use_walk", "drive_fail", "use_drive",
                   "access_dist_m", "tier_min"))) %>%                           # remove old results
  left_join(walk_df,  by = "GEOID") %>%
  left_join(drive_df, by = "GEOID") %>%
  mutate(
    # urban: use walking unless the route failed even though a branch is close
    walk_fail  = dense & is.infinite(walk_bank_m) & dist_bank_m < 1750,
    use_walk   = dense & !is.na(walk_bank_m) & !walk_fail,
    
    # suburban: use driving unless the route failed even though a branch is close
    drive_fail = !dense & is.infinite(drive_bank_m) & dist_bank_m < 8000,       
    use_drive  = !dense & !is.na(drive_bank_m) & !drive_fail,
    
    # distance shown in the popup: network if available, otherwise straight-line
    access_dist_m = case_when(
      use_walk  ~ walk_bank_m,
      use_drive ~ drive_bank_m,
      TRUE      ~ dist_bank_m
    ),
    
    # travel time used for tiers (minutes)
    tier_min = case_when(
      use_walk  & is.finite(walk_min)      ~ walk_min,                          # network walking time
      use_walk  & is.finite(walk_bank_m)   ~ walk_bank_m / 83.3,                # time glitchL estimate at 5 km/h
      use_drive & is.finite(drive_min)     ~ drive_min,                         # network driving time
      use_drive & is.finite(drive_bank_m)  ~ drive_bank_m / 500,                # time glitch: estimate at 30 km/h
      use_walk | use_drive                 ~ Inf,                               # no bank within reach
      dense                                ~ dist_bank_m / 83.3,                # fallback tracts: straight-line estimate
      TRUE                                 ~ dist_bank_m / 500
    ),
    
    # count of branches nearby
    # urban, walk worked = branches within a 1.25 km walk (network)
    # urban, walk failed = branches with a 1.25 km straight-line
    # suburban = branches inside the tract
    banks_near = if_else(use_walk, walk_banks_1k,                               # did the walking network work?
                         if_else(dense, banks_1km,                              # if not, is it urban?
                                 banks_far)),                                   # if not urban then use tract count (suburban)
    
    # same logic for pawnshops and check cashers
    afs_near   = if_else(use_walk, walk_afs_1k,
                         if_else(dense, afs_1km,
                                 afs_far)),
    
    # tiers
    tier = case_when(
      tier_min > 15 & n_main == 0            ~ "Tier 1: Banking Desert",
      afs_near >= 2 & afs_near > banks_near  ~ "Tier 2: AFS Majority",
      TRUE                                   ~ "Served"
    )
  )

# checks
summary(site_df$walk_min)                                                       # walking times (urban)
summary(site_df$drive_min)                                                      # driving times (suburban)
sum(site_df$walk_fail); sum(site_df$drive_fail)                                 # tracts that fell back to straight-line
table(site_df$area_type, site_df$tier)                                          # tier by urban, suburban




# Priority Score

# build a score system (ranked within each tier)
site_df <- site_df %>%
  mutate(underserved = tier != "Served") %>%                                    # not served, deserts and afs majority
  group_by(tier) %>%                                                            # rank deserts against deserts, AFS against AFS
  mutate(
    # percentile rank within the tier: 0 = lowest, 1 = highest 
    poverty_rank = percent_rank(poverty_pct),                                   # need: share of residents in poverty
    afs_rank     = percent_rank(afs_near),                                      # reliance on afs: pawnshops and check cashers nearby
    pop_rank     = percent_rank(total_population),                              # how many people a response would serve
    
    # score = avg of the three ranks; served tracts get no score
    score        = if_else(underserved, (poverty_rank + afs_rank + pop_rank) / 3, NA),
    
    # top 10% of scores within each tier
    top10        = underserved & score >= quantile(score, 0.9, na.rm = TRUE)
  ) %>%
  ungroup()


# how many top-10% tracts per tier
site_df %>% st_drop_geometry() %>% filter(top10) %>% count(tier)                # Total: 51, Desert: 39, AFS: 12



# race check and validate regression results: who lives in top-priority tracts vs. the whole metro

# unweighted mean (mean(race_pct)): every tract counts equally

# weighted mean (weighted.mean(pct, population)): convert tract pcts back into people
# it multiplies each tract's % by its population
# adds them up, and divides by total population
# example: Tract A 1,000 people, 80% Hispanic → 800
#            Tract B 9,000 people, 20% Hispanic → 1,800
#            weighted: 2,600 / 10,000 = 26%  (correct)
#            plain mean: (80 + 20) / 2  = 50%  (wrong: treats a small tract like a big one)

bind_rows(
  site_df %>% st_drop_geometry() %>% filter(top10) %>%
    summarise(black = weighted.mean(black_pct, total_population),               # % Black residents
              hisp  = weighted.mean(hispanic_pct, total_population),            # % Hispanic residents
              n = n(),                                                          # number of tracts
              pop = sum(total_population)) %>%                                  # number of residents
    mutate(group = "Top 10% priority"),   
  
  # the whole metro, for comparison
  site_df %>% st_drop_geometry() %>%
    summarise(black = weighted.mean(black_pct, total_population),
              hisp  = weighted.mean(hispanic_pct, total_population),
              n = n(),
              pop = sum(total_population)) %>%
    mutate(group = "All tracts")
)

# even though race was never part of the score, the places with the worst access and greatest need are mostly Black and Hispanic neighborhoods. That matches the regression.
# among the roughly 324,000 residents of the highest-priority tracts, 62% are Hispanic and 29% are Black, compared with 25% and 15% across the metro.



# tier summaries

# number of tracts in each tier
table(site_df$tier)                                                             # AFS: 373, Desert: 113, Served: 4275

# tracts and residents per tier
site_df %>% st_drop_geometry() %>%
  group_by(tier) %>%                                                       
  summarise(tracts = n(),                                                       # number of tracts
            residents = sum(total_population))                                  # number of people


# underserved tiers split by urban / suburban
site_df %>% st_drop_geometry() %>%
  filter(tier != "Served") %>%                                                  # deserts and afs-majority only
  group_by(tier, area_type) %>%                                                 # group by tier and area type
  summarise(tracts = n(),
            residents = sum(total_population),
            .groups = "drop") %>%                                               # remove grouping after summarizing
  group_by(tier) %>%                                                            # regroup by tier only
  mutate(pct_residents = round(residents / sum(residents) * 100, 1))            # % of the tract's residents
