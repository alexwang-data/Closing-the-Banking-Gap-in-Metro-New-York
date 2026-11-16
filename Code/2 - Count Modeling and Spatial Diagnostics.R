## Part 2: Count Modeling and Spatial Diagnostics_______________________________


# fit a poisson regression and check for overdispersion
# overdispersion = variance > mean
# underdispersion = variance < mean
# equidispersion = variance = mean

# low variance: values are close together (3,3,4,3,2)
# high variance: values are far apart (0,0,1,9,15)

# poisson regression: a poisson model assumes the variance equals the mean
m_pois <- glm(n_afs ~ med_age + poverty_pct + bach_or_higher_pct + public_transit_pct +
                black_pct + asian_pct + hispanic_pct + foreign_born_pct + renter_pct +
                log_total_population,
              family = poisson, data = model_df)


# pearson dispersion ratio: ~1 is fine for Poisson; well above 1 = overdispersed
sum(residuals(m_pois, type = "pearson")^2) / m_pois$df.residual                 # 1.19




# because counts were overdispersed (Pearson dispersion ratio = 1.19), we estimated negative binomial over poisson

# negative binomial model: pawnshops and check cashers
nb_afs <- MASS::glm.nb(
  n_afs ~ med_age + poverty_pct + bach_or_higher_pct + public_transit_pct + black_pct + asian_pct +
    hispanic_pct + foreign_born_pct + renter_pct + log_total_population,
  data = model_df
)

# negative binomial model: banks and credit unions
nb_main <- MASS::glm.nb(
  n_main ~ med_age + poverty_pct + bach_or_higher_pct + public_transit_pct +
    black_pct + asian_pct + hispanic_pct + foreign_born_pct + 
    renter_pct + log_total_population,
  data = model_df
)

# negative binomial model summary: check theta in both
summary(nb_afs)                                                                 # theta = 0.83, under 1 indicates overdispersion relative to a poisson model
summary(nb_main)                                                                # theta = 0.86

# negative binomial model: calculate pseudo R-squared
1 - nb_afs$deviance / nb_afs$null.deviance                                      # 0.19
1 - nb_main$deviance / nb_main$null.deviance                                    # 0.18



# negative binomial model - assumption 1: variance inflation factor
vif(nb_afs)                                                                     # all under 5                     
vif(nb_main)                                                                    # all under 5



# negative binomial model - assumption 2: influential observations
cooks_afs <- cooks.distance(nb_afs)
max(cooks_afs)                                                                  #0.28, the usual concern threshold is 0.5 to 1
sum(cooks_afs > 4 / nrow(model_df))

cooks_main <- cooks.distance(nb_main)
max(cooks_main)                                                                 # 0.09
sum(cooks_main > 4 / nrow(model_df))



# negative binomial model - assumption 3: DHARMa simulated residuals for dispersion and zero inflation

# simulate 1,000 datasets from each fitted model and compare them with the observed count
# if the model fits, real counts land evenly across the simulations

sim_afs <- simulateResiduals(nb_afs, n = 1000)                                  # pawns and check cashers
sim_main <- simulateResiduals(nb_main, n = 1000)                                # banks and credit unions



# plot residuals
plot(sim_afs)                                                                   # diagonal line and flat line
plot(sim_main)                                                                  # the residual vs predicted plot shows small quantile deviations, median and top 75 percentile wiggles a little but small, and make sense given the sample size


# test dispersion after modeling
testDispersion(sim_afs)                                                         # 1 = perfect spread
testDispersion(sim_main)                                                        # 0.94 = very close to a perfect spread, 0.94 vs 1


# test zero inflation
testZeroInflation(sim_afs)                                                      # 1 = the model handles the zeros well (tracts with 0 shops)
testZeroInflation(sim_main)                                                     # 1 = the model handles the zeros well, predicts about the right number of zeros. (some tracts have 0 pawnshops, banks, etc)









## Spatial Autocorrelation

# variables used in the models
model_vars <- c("n_afs", "n_main", "med_age", "poverty_pct", "bach_or_higher_pct",
                "public_transit_pct", "black_pct", "asian_pct", "hispanic_pct",
                "foreign_born_pct", "renter_pct", "log_total_population")

# create a spatial data frame 
spatial_df <- acs_data %>%
  select(GEOID) %>%                                                             # tract IDs + geometry only
  inner_join(model_df, by = "GEOID") %>%                                        # attach model data, keeps only tracts in both (model_df and acs_data)
  filter(if_all(all_of(model_vars), ~ !is.na(.)))                               # drop tracts missing any variable

class(spatial_df)                                                               # model_dfclass(spatial_df), include sf = geometry kept
st_crs(spatial_df)                                                              # ESPG 26918


# create spatial weights / spatial neighbors
neighborlist <- poly2nb(pl = spatial_df,                                        # polygons
                        queen = TRUE,                                           # queen contiguity matrix
                        snap = 1500)                                            # snap distance for island effect, 1500m


# check tracts with no neighbors
sum(card(neighborlist) == 0)                                                    # 1
which(card(neighborlist) == 0)                                                  # tract 1912


# turn the neighbor list into weights used for Moran's I (how much each neighbor counts)

listweights <- nb2listw(neighbours = neighborlist,                              # who neighbors whom (from poly2nb)
                        style = "W",                                            # each tract's neighbor weights sum to 1 (e.g. a tract with 4 neighbors = 1/4 = 0.25)
                        zero.policy = TRUE)                                     # allow tracts with no neighbors (1 tract with no neighbors)


# drop geometry
nb_df <- sf::st_drop_geometry(spatial_df)


# refit the model
var <- "med_age + poverty_pct + bach_or_higher_pct + public_transit_pct +
          black_pct + asian_pct + hispanic_pct + foreign_born_pct + renter_pct +
          log_total_population"

nb_afs  <- MASS::glm.nb(as.formula(paste("n_afs  ~", var)), data = nb_df)
nb_main <- MASS::glm.nb(as.formula(paste("n_main ~", var)), data = nb_df)


# quantile residuals (DHARMa)
set.seed(42)                                                                    # makes results reproducible

sim_afs  <- simulateResiduals(nb_afs,  n = 1000)                                # pawnshops & check cashers 
sim_main <- simulateResiduals(nb_main, n = 1000)                                # banks & credit unions

# convert from 0–1 scale to normal scale
spatial_df$resid_afs  <- qnorm(pmin(pmax(residuals(sim_afs),  0.001), 0.999))
spatial_df$resid_main <- qnorm(pmin(pmax(residuals(sim_main), 0.001), 0.999))

# Moran's I on the residuals
moran_afs  <- moran.test(spatial_df$resid_afs,  listweights, zero.policy = TRUE)
moran_main <- moran.test(spatial_df$resid_main, listweights, zero.policy = TRUE)

moran_afs                                                                       # p = 0.6, not significant
moran_main                                                                      # p = 0.3, not significant


# Moran’s I computed on randomized quantile residuals (Dunn & Smyth, 1996) indicated no remaining spatial autocorrelation in either the mainstream (I ≈ 0, p = 0.44) or afs model (I ≈ 0, p = 0.47).





# Negative Binomial Regression Results Table

# rescale the data: percentage variables per 10 points, age per 10 years
# after dividing by 10, 1 unit means 10 percentage points
nb_df_10 <- nb_df %>%
  mutate(across(c(poverty_pct, bach_or_higher_pct, public_transit_pct,
                  black_pct, asian_pct, hispanic_pct,
                  foreign_born_pct, renter_pct, med_age),
                ~ .x / 10))

# refit on rescaled data
nb_main_10 <- MASS::glm.nb(as.formula(paste("n_main ~", var)), data = nb_df_10)
nb_afs_10  <- MASS::glm.nb(as.formula(paste("n_afs  ~", var)), data = nb_df_10)

# add DHARMa diagnostics
set.seed(42)

sim_main <- simulateResiduals(nb_main_10, n = 1000)                             # banks & credit unions
sim_afs  <- simulateResiduals(nb_afs_10,  n = 1000)                             # pawnshops & check cashers

disp_main <- testDispersion(sim_main,    plot = FALSE)
disp_afs  <- testDispersion(sim_afs,     plot = FALSE)
zi_main   <- testZeroInflation(sim_main, plot = FALSE)
zi_afs    <- testZeroInflation(sim_afs,  plot = FALSE)



# extra rows for the table

# deviance explained, a pseudo-R^2 for count models
dev_exp  <- function(m) 1 - m$deviance / m$null.deviance

# format a DHARMa test resukt
fmt_test <- function(t) sprintf("%.2f (p = %.2f)", t$statistic, t$p.value)

# build a small table: one row for statistic, one column per model
extra_gof_nb <- data.frame(
  term = c("N (tracts)", "AIC", "Theta (dispersion)", "Deviance explained",
           "DHARMa dispersion", "Zero-inflation ratio"),
  "Banks & Credit Unions" = c(
    nobs(nb_main_10),                                                           # number of tracts
    sprintf("%.1f", AIC(nb_main_10)),                                           # AIC, 1 decimal, lower = better fit
    sprintf("%.3f", nb_main_10$theta),                                          # NB dispersion parameter, 3 decimals
    sprintf("%.1f%%", 100 * dev_exp(nb_main_10)),                               # deviance explained, as a percent
    fmt_test(disp_main),                                                        # DHARMa dispersion ratio + p-value
    fmt_test(zi_main)                                                           # zero-inflation ratio + p-value
  ),
  "Pawnshops & Check Cashers" = c(
    nobs(nb_afs_10),
    sprintf("%.1f", AIC(nb_afs_10)),
    sprintf("%.3f", nb_afs_10$theta),
    sprintf("%.1f%%", 100 * dev_exp(nb_afs_10)),
    fmt_test(disp_afs),
    fmt_test(zi_afs)
  ),
  check.names = FALSE                                                           # keep column names as written (spaces and & allowed)
)

# build a table with gt
nb_table <- modelsummary(
  list("Banks & Credit Unions"     = nb_main_10,
       "Pawnshops & Check Cashers" = nb_afs_10),
  exponentiate = TRUE,
  statistic    = "[{conf.low}, {conf.high}]",
  stars        = TRUE,
  fmt          = 2,
  coef_map = c(
    "black_pct"            = "Black",
    "hispanic_pct"         = "Hispanic",
    "asian_pct"            = "Asian",
    "foreign_born_pct"     = "Foreign-born",
    "poverty_pct"          = "Poverty",
    "bach_or_higher_pct"   = "Bachelor's Degree +",
    "public_transit_pct"   = "Public Transit Commuters",
    "renter_pct"           = "Renter-Occupied Housing",
    "med_age"              = "Median Age (per 10 years)",
    "log_total_population" = "Log Total Population"
  ),
  gof_omit = ".*",
  add_rows = extra_gof_nb,
  output   = "gt"
) %>%
  tab_header(
    title    = "Negative Binomial Regression Results",
    subtitle = "Incidence rate ratios per 10-percentage-point increase"
  ) %>%
  tab_source_note(
    "Note: Values above 1 indicate more establishments; below 1, fewer (e.g., 0.86 = 14% fewer). 95% confidence intervals in brackets. Percentage variables scaled per 10 points; median age per 10 years. White (%) is the omitted reference category. DHARMa dispersion and zero-inflation ratios near 1 with non-significant p-values indicate adequate fit."
  ) %>%
  tab_source_note(
    "Source: FDIC Summary of Deposits, 2023; NCUA Call Report Data, 2023; NYC DCWP; NYS DFS; NJ DOBI; OpenStreetMap, 2026; U.S. Census Bureau, 2019–2023 ACS 5-Year Estimates."
  ) %>%
  tab_style(
    style = cell_text(size = 16, weight = "bold"),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = cell_text(size = 11, color = "grey40"),
    locations = cells_title(groups = "subtitle")
  ) %>%
  tab_style(
    style = cell_text(size = 11),
    locations = cells_body()
  ) %>%
  tab_style(
    style = cell_text(color = "grey50", align = "left"),
    locations = cells_source_notes()
  ) %>%
  tab_options(
    table.font.names = "Times New Roman",
    heading.align = "center",
    source_notes.font.size = 9,
    table.width = px(650)
  )

nb_table

gtsave(nb_table, "nb_table.png")



# Moran's I Scatterplots

# build the labels for the plots
lab_afs  <- sprintf("Moran's I = %.3f (p = %.2f)",
                    moran_afs$estimate[1],  moran_afs$p.value)
lab_main <- sprintf("Moran's I = %.3f (p = %.2f)",
                    moran_main$estimate[1], moran_main$p.value)


# standardize for proper quadrants around 0 (so 0 means average, positive means above average, and negative means below average)
# in the scatterplot, if points cluster, for ex, top-right or bottom-left, then the model's mistakes are clustered in space
# if the points form a round cloud with a flat line, the model's surprises are scattered randomly, with no geographic clusters


# standardize residuals and store as a new column
spatial_df$resid_afs_std      <- as.numeric(scale(spatial_df$resid_afs))
# calculate spatial lag of residuals
spatial_df$lag_resid_afs_std  <- lag.listw(listweights, spatial_df$resid_afs_std,  zero.policy = TRUE)


# standardize residuals and store as a new column
spatial_df$resid_main_std     <- as.numeric(scale(spatial_df$resid_main))
# calculate spatial lag of residuals
spatial_df$lag_resid_main_std <- lag.listw(listweights, spatial_df$resid_main_std, zero.policy = TRUE)


# check slope of lag vs residual should equal Moran's I
# use lm to fit a straight line through the points
coef(lm(lag_resid_afs_std  ~ resid_afs_std,  data = spatial_df))[2]             # -0.0016 (Moran's I)
coef(lm(lag_resid_main_std ~ resid_main_std, data = spatial_df))[2]             # 0.0023  (Moran's I)


# afs residuals
moran_resid_afs <- ggplot(spatial_df,
                          aes(x = resid_afs_std, y = lag_resid_afs_std)) +
  geom_pointdensity(size = 0.6, shape = 16, show.legend = FALSE) +              # dense area = darker
  scale_color_gradient(low = "grey85", high = "grey15") +                       # light = sparse, dark = dense
  geom_smooth(method = "lm",
              color = scales::alpha("#C0583A", 0.6), se = FALSE) +              # fit a regression line
  coord_cartesian(xlim = c(-3.5, 3.5), ylim = c(-2, 2)) +                       # zoom the view to a symmetric range without dropping any data
  annotate("text", x = Inf, y = -Inf, hjust = 1.05, vjust = -3,
           label = lab_afs, family = "Tahoma", size = 3) +
  labs(
    title = "Pawnshops & Check Cashers",
    x = "Standardized Residuals",
    y = "Spatially Lagged Residuals"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Tahoma", size = 8),
    plot.title = element_text(hjust = 0.5)
  )


# mainstream residuals
moran_resid_main <- ggplot(spatial_df,
                           aes(x = resid_main_std, y = lag_resid_main_std)) +
  geom_pointdensity(size = 0.6, shape = 16, show.legend = FALSE) +              # dense area = darker
  scale_color_gradient(low = "grey85", high = "grey15") +                       # light = sparse, dark = dense
  geom_smooth(method = "lm",
              color = scales::alpha("#5C7A98", 0.6), se = FALSE) +              # fit a regression line
  coord_cartesian(xlim = c(-3.5, 3.5), ylim = c(-2, 2)) +                       # zoom the view to a symmetric range without dropping any data
  annotate("text", x = Inf, y = -Inf, hjust = 1.05, vjust = -3,
           label = lab_main, family = "Tahoma", size = 3) +
  labs(
    title = "Banks & Credit Unions",
    x = "Standardized Residuals",
    y = "Spatially Lagged Residuals"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Tahoma", size = 8),
    plot.title = element_text(hjust = 0.5)
  )


# assemble the plots with patchwork
moran_plots <- (moran_resid_main | moran_resid_afs) +
  plot_annotation(
    title = "Spatial Autocorrelation of Model Residuals",
    caption = "Randomized quantile residuals (DHARMa) from negative binomial models.\nSource: FDIC Summary of Deposits, 2023; NCUA Call Report Data, 2023; NYC DCWP; NYS DFS; NJ DOBI; OpenStreetMap, 2026; U.S. Census Bureau, 2019–2023 ACS 5-Year Estimates.",
    theme = theme(
      plot.title = element_text(family = "Tahoma", size = 14,
                                hjust = 0.5, face = "bold"),
      plot.caption = element_text(margin = margin(t = 20),
                                  family = "Tahoma", size = 6, hjust = 1)
    )
  )

moran_plots

ggsave("moran_residual_plots.png", moran_plots, width = 8, height = 4, dpi = 300, bg = "white")






# 2x2 choropleth (main + afs / black pop + hispanic pop)

# categories
spatial_df <- spatial_df %>%
  mutate(
    bank_rate = n_main / total_population * 10000,                              # rates per 10,000 residents, so big and small tracts are comparable
    afs_rate  = n_afs  / total_population * 10000,
    bank_cat  = cut(bank_rate, breaks = c(-Inf, 0, 2, 5, 10, Inf),              # group rates into 5 classes
                    labels = c("0", "0–2", "2–5", "5–10", "10+")),
    afs_cat   = cut(afs_rate,  breaks = c(-Inf, 0, 2, 5, 10, Inf),
                    labels = c("0", "0–2", "2–5", "5–10", "10+")),
    black_cat = cut(black_pct,    breaks = c(-Inf, 20, 40, 60, 80, Inf),        # group population shares into 20-point classes
                    labels = c("<20", "20–40", "40–60", "60–80", "80+")),
    hisp_cat  = cut(hispanic_pct, breaks = c(-Inf, 20, 40, 60, 80, Inf),
                    labels = c("<20", "20–40", "40–60", "60–80", "80+"))
  )

# palette
zero_col <- "#F2F2F2"                                                           # very light gray for 0 / <20%

# category names
rate_lv <- c("0", "0–2", "2–5", "5–10", "10+")                                  # for the rate maps
pct_lv  <- c("<20", "20–40", "40–60", "60–80", "80+")                           # for the pct maps

blues   <- setNames(c(zero_col,
                      "#D6DEE6", "#9DB0C3", "#5C7A98", "#2E4A66"), rate_lv)     # banks: slate
reds    <- setNames(c(zero_col,
                      "#F2D3C7", "#E09A80", "#C0583A", "#7A2E1A"), rate_lv)     # fringe: terracotta
charcoal <- setNames(c(zero_col,
                       "#D0D0D0", "#A3A3A3", "#6B6B6B", "#2B2B2B"), pct_lv)     # Black % and Hispanic %: charcoal


# zoom box, outlines, labels

# bounding box of the five NYC boroughs (Bronx, Kings, New York, Queens, Richmond)
# used to zoom every map to the ame area
nyc_box <- st_bbox(spatial_df %>%
                     filter(substr(GEOID, 1, 5) %in%
                              c("36005", "36047", "36061", "36081", "36085")))

# county boundaries for NY and NJ
county_lines <- counties(state = c("NY", "NJ"), cb = TRUE, year = 2023) %>%
  st_transform(st_crs(spatial_df))                                              # match CRS

# place labels, positioned by longitude/latitude
place_labels <- data.frame(
  name = c("Bronx", "Manhattan", "Queens", "Brooklyn",
           "Staten Island", "Newark", "Jersey City"),
  x = c(-73.86, -73.965, -73.80, -73.95, -74.15, -74.17, -74.07),               # long
  y = c(40.85, 40.78, 40.72, 40.65, 40.58, 40.73, 40.72)                        # lat
) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%                                # turn into points (x= long, y = lay)
  st_transform(st_crs(spatial_df))                                              # match CRS


# one map function for all four panels
make_map <- function(fill_var, palette, title, legend_title) {
  ggplot() +
    geom_sf(data = spatial_df, aes(fill = .data[[fill_var]]), color = NA) +     # tracts, no borders
    geom_sf(data = county_lines, fill = NA,
            color = "#2B2B2B", linewidth = 0.2) +                               # county outlines
    geom_sf_text(data = place_labels, aes(label = name), color = "#2B2B2B",
                 family = "Tahoma", size = 2.2, fontface = "bold") +            # place names
    scale_fill_manual(values = palette, name = legend_title, drop = FALSE) +    # keep all classes in legend
    coord_sf(xlim = c(nyc_box["xmin"], nyc_box["xmax"]),                        # zoom to NYC
             ylim = c(nyc_box["ymin"], nyc_box["ymax"]),
             expand = FALSE) +
    labs(title = title) +
    theme_void()                                                                # no axes or grid lines
}

# the four panels
p_banks <- make_map("bank_cat",  blues,   "Banks & Credit Unions",     "per 10k")
p_afs   <- make_map("afs_cat",   reds,    "Pawnshops & Check Cashers", "per 10k")
p_black <- make_map("black_cat", charcoal, "Black Population",          "% of tract")
p_hisp  <- make_map("hisp_cat",  charcoal,  "Hispanic Population",       "% of tract")


# 2x2 layout
fig_2x2 <- (p_banks | p_afs) / (p_black | p_hisp) +
  plot_annotation(
    title      = "",
    caption    = "Census tracts in the New York–Newark–Jersey City Core-Based Statistical Area (CBSA 35620), zoomed to New York City and adjacent New Jersey. Gray = none or <20%; white = water or excluded tracts.\nSource: FDIC Summary of Deposits, 2023; NCUA Call Report Data, 2023; NYC DCWP; NYS DFS; NJ DOBI; OpenStreetMap, 2026; U.S. Census Bureau, 2019–2023 ACS 5-Year Estimates.",
    tag_levels = "A"                                                            # label panels A, B, C, D
  ) &
  theme(
    text             = element_text(family = "Tahoma", size = 8, color = "#2B2B2B"),
    plot.title       = element_text(hjust = 0.5),
    plot.caption     = element_text(hjust = 0, size = 6),
    plot.tag         = element_text(face = "bold")
  )

fig_2x2

ggsave("fig_mainstream_vs_fringe_2x2.png", fig_2x2, width = 10, height = 10, dpi = 300)



# freeze the data for reproducibility - saveRDS() and readRDS()
saveRDS(spatial_df, "spatial_df_final.rds")
saveRDS(banks_sf, "banks_final.rds")
saveRDS(cu_sf, "cu_final.rds")
saveRDS(afs_sf, "afs_final.rds")
