## Part 1: Data Collection and Geoprocessing____________________________________


# define ACS variables
acs_variables <- c(
  white = "B03002_003E",
  black = "B03002_004E",
  asian = "B03002_006E",
  hispanic = "B03002_012E",
  med_income = "B19013_001E",
  med_age = "B01002_001E",
  poverty_pct = "S1701_C03_001E",
  poverty_universe = "S1701_C01_001E",    
  total_population = "B01003_001E",
  foreign_born_population = "B05002_013E",
  population_25y_abv = "B15003_001E",                                           # degrees / pop_25 +
  bach_degree = "B15003_022E",
  master_degree = "B15003_023E",
  prof_degree = "B15003_024E",
  doctorate_degree = "B15003_025E", 
  total_workers = "B08301_001E",                                                # from the transportation table
  public_transportation = "B08301_010E",                                        # public transportation / total workers
  labor_force = "B23025_003E",             
  unemployed = "B23025_005E",                                                   # unemployed / labor_force
  renter = "B25003_003E",                                                       # renter / occupied_units
  occupied_units = "B25003_001E"
)

# pull trace-level ACS data: yr 2019 - 2023
acs_data <- get_acs(geography = "tract",
                    state = c("NY", "NJ", "PA"),
                    variables = acs_variables,
                    output = "wide", 
                    survey = "acs5",
                    year = 2023, 
                    geometry = TRUE)

# drop ACS margins of errors
acs_data <- acs_data %>%
  select(-ends_with("M", ignore.case = FALSE))                                  # drop columns that ends with capital M, uppercase only


# ACS projection: 26918
acs_data <- st_transform(acs_data, crs = 26918)
st_crs(acs_data)

# get the CBSA boundaries: 35620 NY metropolitan
cbsa_boundary <- core_based_statistical_areas(cb = TRUE, year = 2023) %>%
  filter(CBSAFP == "35620")

# CBSA projection: 26918
cbsa_boundary <- st_transform(cbsa_boundary, crs = 26918)
st_crs(cbsa_boundary)

# clip census tracts that are spatially within with CBSA boundaries
acs_data <- st_filter(acs_data, cbsa_boundary, .predicate = st_within)

# visualize the NY CBSA region
ggplot(acs_data) +
  geom_sf(fill = "gray", color = "black", size = .05) + 
  coord_sf(crs = 26918) +
  theme_minimal() +
  labs(title = "Census tracts in the NY Metropolitan CBSA region")



# NYC pawnshops: pull NYC pawn shops via Socrata API
pawn_nyc <- soc_read(
  "https://data.cityofnewyork.us/resource/w7w3-xahh.json"
)

# NYC pawnshops: filter to NYC pawnshops with active license
pawn_nyc_active <- pawn_nyc %>%
  filter(license_status == "Active",
         business_category == "Pawnbroker")

# NYC pawnshops: check missing coordinates; no latitude or no longitude
sum(is.na(pawn_nyc_active$latitude) | is.na(pawn_nyc_active$longitude))         #8 without coordinates

# NYC pawnshops: filter out 8 pawnshops with missing coordinates
pawn_nyc_active_geo <- pawn_nyc_active %>%
  filter(!is.na(latitude), !is.na(longitude))



# NYS pawnshops via OSM Overpass API

# NYS pawnshops: get counties
nys_counties <- counties(state = "NY", cb = TRUE, year = 2023) %>%
  filter(NAME %in% c("Nassau", "Suffolk", "Westchester",
                     "Rockland", "Putnam", "Orange"))

# NYS pawnshops: define the area and draw a rectangle around the sf object; return the smallest rectangle containing all six counties
bb <- st_bbox(nys_counties)

# NYS pawnshops: query OpenStreetMap fro pawnshops in that rectangle
pawn_osm <- opq(bbox = c(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])) %>%   # build the query for this area
  add_osm_feature(key = "shop", value = "pawnbroker") %>%                       # query pawnshops
  osmdata_sf()                                                                  # send to the server, return sf object


# NYS pawnshops: combine shops mapped as points and as building outlines (in OSM, some are points and some are outlines)
pawn_nys <- bind_rows(
  pawn_osm$osm_points,                                                          # mapped as a single point
  st_centroid(pawn_osm$osm_polygons)                                            # mapped as a building outline, use centroid
) %>%
  st_transform(st_crs(nys_counties)) %>%                                        # match CRS, NYS counties
  st_join(nys_counties, join = st_within) %>%                                   # tag each shop with the county it falls in
  filter(!is.na(NAME))                                                          # drop anything outside the 6 counties

# NYS pawnshops: extract longitude and latitude, then drop geometry
pawn_nys <- pawn_nys %>%
  mutate(longitude = st_coordinates(.)[, 1],                                    # column 1 = x = longitude
         latitude = st_coordinates(.)[, 2]) %>%                                 # column 2 = y = latitude
  st_drop_geometry()




# define a two-pass geocoding function: census first, OpenStreetMap for the failures
geocode_two_pass <- function(df, address = "full_address", label = "data") {
  
  df <- df %>% mutate(.addr = .data[[address]])                                 # copy address into a new column called .addr, use [[]] when the column name is stored in a variable, inside a function
  
  # pass 1: census
  first <- df %>%
    geocode(address = .addr, method = "census", full_results = TRUE)
  
  ok <- first %>% filter(!is.na(lat))
  failed <- first %>% filter(is.na(lat))
  
  # pass 2: retry failures with OSM (often more forgiving when the address doesn't match the street data exactly)
  retry <- if (nrow(failed) > 0) {
    failed %>%
      select(-lat, -long) %>%
      geocode(address = .addr, method = "osm")
  } else failed
  
  # combine both passes; drop unmatched
  out <- bind_rows(ok, retry)
  dropped <- sum(is.na(out$lat))
  
  message(label, ": ", nrow(df), " addresses | Census matched ", nrow(ok),
          " | OSM matched ", nrow(retry) - sum(is.na(retry$lat)),
          " | dropped ", dropped)
  
  out %>% filter(!is.na(lat)) %>%
    select(-.addr)                                                              # remove the helper column
}




# NYS + NYC check cashers: read csv
check_nys <- read.csv("ny_checkcashers.csv")

# NYS + NYC check cashers: geocode addresses
check_nys_geo <- geocode_two_pass(check_nys, label = "NY Check Cashers")





# NYC pawnshops: standardize columns
pawn_nyc_active_geo <- pawn_nyc_active_geo %>%
  transmute(
    category = "Pawnbroker",
    business_name = business_name,
    address = paste(`address_building`, `address_street_name`),
    city = "New York",
    state = "NY",
    zip_code = address_zip,
    county = address_borough,
    latitude = latitude,
    longitude = longitude,
    source = "NYC DCWP"
  )

# NYS pawnshops: standardize columns
pawn_nys <- pawn_nys %>%
  transmute(
    category = "Pawnbroker",
    business_name = name,
    address = paste(`addr:housenumber`, `addr:street`),
    city = `addr:city`,
    state = "NY",
    zip_code = `addr:postcode`,
    county = NAME,
    latitude = latitude,
    longitude = longitude,
    source = "OpenStreetMap"
  )

# NYC + NYS check cashers: standardize columns
check_nys_geo <- check_nys_geo %>%
  transmute(
    category = "Check Cashers",
    business_name = business_name,
    address = address,
    city = city,
    state = "NY",
    zip_code = zip,
    county = city,
    latitude = lat,
    longitude = long,
    source = "NYS DFS"
  )

# NY alternative financial services: bind rows in NYC pawn, NYS pawn, NYC + NYS check cashers
ny_afs_all <- bind_rows(pawn_nyc_active_geo, pawn_nys, check_nys_geo)



# NJ pawnshops: read csv
pawn_nj_active <- read.csv("nj_pawnbrokers_dedup.csv")

# NJ pawnshops: geocode addresses
pawn_nj_active_geo <- geocode_two_pass(pawn_nj_active, label = "NJ Pawnshops")


# NJ check cashers: read csv
check_nj <- read.csv("nj_checkcashers_dedup.csv")

# NJ check cashers: geocode addresses
check_nj_geo <- geocode_two_pass(check_nj, label = "NJ check cashers") %>%
  distinct(dedup_location_id, .keep_all = TRUE)                                 # keep only one row per location ID, each location is counted only once
# .keep_all = TRUE keeps all other columns, prevent them from being dropped when using distinct()

# NJ pwanshops: standardize columns
pawn_nj_active_geo <- pawn_nj_active_geo %>%
  transmute(
    category = "Pawnbroker",
    business_name = business_name,
    address = address,
    city = city,
    state = "NJ",
    zip_code = zip,
    county = city,
    latitude = lat,
    longitude = long,
    source = "NJ DOBI"
  )

# NJ check cashers: standardize columns
check_nj_geo <- check_nj_geo %>%
  transmute(
    category = "Check Cashers",
    business_name = business_name,
    address = address,
    city = city,
    state = "NJ",
    zip_code = zip,
    county = city,
    latitude = lat,
    longitude = long,
    source = "NJ DOBI"
  )

# convert zipcode into character format
pawn_nj_active_geo <- pawn_nj_active_geo %>%
  mutate(zip_code = as.character(zip_code))

# NJ alternative financial services: bind rows in NJ pawn, NJ check cashers
nj_afs_all <- bind_rows(pawn_nj_active_geo, check_nj_geo)


# NY CBSA alternative financial services: compiled dataset for afs (NY + NJ)
cbsa_afs_all <- bind_rows(ny_afs_all, nj_afs_all)



# NY CBSA banks: FDIC pagination loop, get the results page by page until everything is downloaded
get_fdic_sod <- function(state, year = 2023) {
  offset <- 0
  limit <- 10000                                                                # row per page
  out <- list()
  repeat {
    resp <- GET("https://banks.data.fdic.gov/api/sod",
                query = list(
                  filters = paste0("STALPBR:", state, " AND YEAR:", year),
                  # columns to return:
                  # UNINUMBR = unique branch ID; NAMEFULL = bank name; NAMEBR = branch name;
                  # ADDRESBR, CITYBR, STALPBR, ZIPBR = branch address; CNTYNAMB = county;
                  # STCNTYBR = state+county FIPS; SIMS_LATITUDE/LONGITUDE = coordinates;
                  # DEPSUMBR = branch deposits; YEAR = data year
                  fields = "UNINUMBR, NAMEFULL, NAMEBR, ADDRESBR, CITYBR,
                  STALPBR, ZIPBR, CNTYNAMB, STCNTYBR, SIMS_LATITUDE,
                  SIMS_LONGITUDE, DEPSUMBR, YEAR",
                  limit = limit, offset = offset, format = "json"))
    
    # convert the JSON response into an R data frame
    parsed <- fromJSON(content(resp, "text", encoding = "UTF-8"),
                       flatten = TRUE)
    dat <- parsed$data
    
    # stop is this page is empty (no more results)
    if (is.null(dat) || nrow(dat) == 0) break
    
    # remove the "data." prefix (e.g. data.NAMEFULL)
    names(dat) <- sub("^data\\.", "", names(dat))
    
    # save the page
    out[[length(out) + 1]] <- dat
    
    # a partial page means it was the last one
    if (nrow(dat) < limit) break
    
    # otherwise, move to the next page
    offset <- offset + limit
  } 
  
  bind_rows(out)                                                                # stack all pages into one table
}



# NY, NJ, PA banks: store the data
ny_banks <- get_fdic_sod("NY")
nj_banks <- get_fdic_sod("NJ")
pa_banks <- get_fdic_sod("PA")

# check duplicate using the primary key
n_distinct(ny_banks$ID)                                                         #4176
n_distinct(nj_banks$ID)                                                         #2383
n_distinct(pa_banks$ID)                                                         #3443

# NY CBSA banks: bind rows in NY banks, NJ banks, PA banks
cbsa_banks_all <- bind_rows(ny_banks, nj_banks, pa_banks)

# NY CBSA banks: standardize columns
cbsa_banks_all <- cbsa_banks_all %>%
  transmute(
    category = "Banks",
    business_name = NAMEFULL,
    address = ADDRESBR,
    city = CITYBR,
    state = STALPBR,
    zip_code = ZIPBR,
    county = CNTYNAMB,
    latitude = SIMS_LATITUDE,
    longitude = SIMS_LONGITUDE,
    source = "FDIC"
  )


# NY CBSA credit unions: define url
url <- "https://ncua.gov/files/publications/analysis/call-report-data-2023-06.zip"
destfile <- "ncua_2023_06.zip"

# NY CBSA credit unions: download and unzip
download.file(url, destfile = destfile, mode = "wb", method = "libcurl")
unzip("ncua_2023_06.zip")

# NY CBSA credit unions: check all files within the zip
list.files("ncua_2023_06")

# NY CBSA credit unions: select and download the data file
branches <- read.csv("ncua_2023_06/Credit Union Branch Information.txt",
                     stringsAsFactors = FALSE)
names(branches)
head(branches, 3)

# NY CBSA credit unions: filter to NY, NJ, PA
cbsa_credit_union <- branches %>%
  filter(PhysicalAddressStateCode %in% c("NY", "NJ", "PA"))

# NY CBSA credit unions: create an address column for geocoding
cbsa_credit_union <- cbsa_credit_union %>%
  mutate(full_address = paste(PhysicalAddressLine1, PhysicalAddressCity,
                              PhysicalAddressStateCode, PhysicalAddressPostalCode,
                              sep = ", "))


# NY CBSA credit unions: geocode addresses
cbsa_credit_union_geo <- geocode_two_pass(cbsa_credit_union, label = "NY Credit Unions")


# NY CBSA credit unions: check office type
table(cbsa_credit_union_geo$SiteTypeName)

# NY CBSA credit unions: filter to branch office
cbsa_credit_union_geo <- cbsa_credit_union_geo %>%
  filter(SiteTypeName %in% "Branch Office")


# NY CBSA credit unions: standardize columns
cbsa_credit_union_all <- cbsa_credit_union_geo %>%
  transmute(
    category = "Credit Unions",
    business_name = CU_NAME,
    address = PhysicalAddressLine1,
    city = PhysicalAddressCity,
    state = PhysicalAddressStateCode,
    zip_code = PhysicalAddressPostalCode,
    county = PhysicalAddressCountyName,
    latitude = lat,
    longitude = long,
    source = "NCUN"
  )



# master and model data frames

# spatial point conversion: convert tables to spatial points (geometry) and match projection

# AFS
afs_sf <- st_as_sf(x = cbsa_afs_all,
                   coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(st_crs(acs_data))                                                # match projection

# banks
banks_sf <- st_as_sf(x = cbsa_banks_all,
                     coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(st_crs(acs_data))                                                # match projection

# credit unions
cu_sf <- st_as_sf(x = cbsa_credit_union_all,
                  coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(st_crs(acs_data))                                                # match projection


# spatial join: tag each point with the tract it falls in, only add GEOID and the geometry

# AFS
afs_join <- st_join(afs_sf,   acs_data["GEOID"],
                    join = st_within)                                           # spatial join

# banks
banks_join <- st_join(banks_sf, acs_data["GEOID"],
                      join = st_within)                                           # spatial join

# credit unions
cu_join <- st_join(cu_sf,    acs_data["GEOID"],
                   join = st_within)                                           # spatial join


# count establishments: drop geometry and count each category per tract, based on GEOID

# pawnshops
pawn_counts <- afs_join %>% st_drop_geometry() %>%
  filter(category == "Pawnbroker") %>% 
  count(GEOID, name = "n_pawn")                                                 # count pawnshops

# check cashers
cc_counts <- afs_join %>% st_drop_geometry() %>%
  filter(category == "Check Cashers") %>% 
  count(GEOID, name = "n_cc")                                                   # count check cashers

# banks
bank_counts <- banks_join %>% st_drop_geometry() %>%
  count(GEOID, name = "n_banks")                                                # count banks

# credit unions
cu_counts <- cu_join %>% st_drop_geometry() %>%
  count(GEOID, name = "n_cu")                                                   # count credit unions


# attach counts to ACS: join counts back to tract polygons, fill tracts with 0, compute for an exploratory index
acs_data <- acs_data %>%
  left_join(pawn_counts, by = "GEOID") %>%
  left_join(cc_counts,   by = "GEOID") %>%
  left_join(bank_counts, by = "GEOID") %>%
  left_join(cu_counts,   by = "GEOID") %>%
  mutate(across(c(n_pawn, n_cc, n_banks, n_cu), ~ replace_na(., 0))) %>%        # no match = 0, not NA
  mutate(FDI = (n_pawn + n_cc + 1) / (n_banks + n_cu + 1))                      # index, (+1 avoids dividing by 0)

# exploratory bank desert: tracts with no bank or credit union inside
acs_data <- acs_data %>%
  mutate(bank_desert = (n_banks + n_cu) == 0)

sum(acs_data$bank_desert)                                                       # how many tracts have zero mainstream banking
nrow(acs_data)                                                                  # total tracts


# map the index
ggplot(acs_data) +
  geom_sf(aes(fill = FDI), color = NA) +
  scale_fill_gradient2(
    midpoint = 0,                                                               # 0 on the log scale = FDI of 1
    low = "#2c7bb6", mid = "grey90", high = "#d7191c",
    trans = "log",
    limits = c(1/8, 8),                                                         # symmetric around 1
    breaks = c(1/8, 1/4, 1/2, 1, 2, 4, 8),
    labels = c("1/8", "1/4", "1/2", "1", "2", "4", "8"),
    oob = scales::squish,
    name = "FDI"
  ) +
  theme_void() +
  labs(title = "Financial Divide Index by Census Tract",
       subtitle = "NY CBSA",
       caption = "FDI = (Pawnshops + Check Cashers + 1) / (Banks + Credit Unions + 1)") +
  theme(text = element_text(family = "Times New Roman", size = 8),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

max(acs_data$FDI)                                                               #6
median(acs_data$FDI)                                                            #1
min(acs_data$FDI)                                                               #0.04


# bank sanity check: are branch points in the right place?
ggplot() +
  geom_sf(data = acs_data, fill = NA, color = "grey70", linewidth = 0.1) +      # tract outlines (no fill)
  geom_sf(data = banks_sf, color = "red", size = 0.3) +                         # bank branches as small red points
  theme_minimal()



# build the master data frame

# define a function: calculate percentage, or NA when the denominator is 0
pct <- function(num, den) if_else(den > 0, num / den * 100, NA_real_)

master <- acs_data %>%
  mutate(
    white_pct = pct(white, total_population),
    black_pct = pct(black, total_population),
    asian_pct = pct(asian, total_population),
    hispanic_pct = pct(hispanic, total_population),
    bach_or_higher_pct = pct(bach_degree + master_degree + prof_degree + doctorate_degree, 
                             population_25y_abv),
    public_transit_pct = pct(public_transportation, total_workers),
    unemployment_pct = pct(unemployed, labor_force),
    foreign_born_pct = pct(foreign_born_population, total_population),
    renter_pct = pct(renter, occupied_units),
    area_land = as.numeric(st_area(geometry)),                                  # square meters
    population_density = total_population / (area_land / 2589988)               # 2,589,988 m² = 1 sq mile
  )


# model data frame: drop tracts and filter outliers
model_df <- master %>%
  filter(total_population >= 500,                                               # drop parks, airports, non-residential tracts
         labor_force >= 50,                                                     # drop tracts with too few workers
         poverty_universe >= 100) %>%                                           # drop outlier tracts (not eligible for poverty)
  select(GEOID, FDI, total_population, population_density, med_age, n_pawn,
         n_cc, n_banks, n_cu, ends_with("_pct")) %>%
  mutate(log_FDI = log(FDI),                                                    # log FDI
         log_population_density = log(population_density),                      # log population density
         log_total_population = log(total_population),                          # log total population
         n_afs  = n_pawn + n_cc,                                                # afs counts
         n_main = n_banks + n_cu) %>%                                           # mainstream counts
  st_drop_geometry()


# how many tracts were removed?
nrow(master) - nrow(model_df)                                                   # 141 were dropped
