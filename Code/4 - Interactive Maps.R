## Part 4: Interactive Maps_____________________________________________________


# web maps (mapgl) need longitude/latitude (EPSG 4326), so convert from 26918 to 4326 later

# add CBSA official metro-level income
metro_income <- get_acs(geography = "cbsa",
                        variables = "B19013_001",                               # median household income
                        year = 2023,
                        survey = "acs5") %>%
  filter(GEOID == "35620") %>%                                                  # CBSA
  pull(estimate)                                                                # extract the number

metro_income                                                                    # $97334


# metro avgs for the popup (population-weighted)
# e.g. metro poverty % = (people in poverty in every tract, added up) / (people in every tract, added up)
metro <- site_df %>%
  st_drop_geometry() %>%
  summarise(across(c(poverty_pct, bach_or_higher_pct, black_pct, hispanic_pct,
                     asian_pct, foreign_born_pct, renter_pct, public_transit_pct),
                   ~ weighted.mean(.x, total_population, na.rm = TRUE)))        # .x = each column




# metro share of car-free households (household-weighted)
metro_car_free <- site_df %>%
  st_drop_geometry() %>%
  summarise(x = weighted.mean(pct_no_vehicle, households, na.rm = TRUE)) %>%
  pull(x)                                                                       # extracts the number

metro_car_free                                                                  # about 31%


# county names from GEOID, for the popup header
# 5 digit FIPS
county_lookup <- tigris::fips_codes %>%
  transmute(cfips = paste0(state_code, county_code),                            # e.g. 36 + 005 + 36005
            county = paste0(county, ", ", state))                               # e.g. Bronx County, NY




# HTML, CSS, R


# popup table
# label on the left, value on the right
# metro avg in gray when metro_val is given
row <- function(label, value, metro_val = NULL) {
  m <- if (is.null(metro_val)) "" else
    sprintf("<span style='color:#999;font-size:10px;'> (metro avg %.0f%%)</span>", metro_val)
  sprintf("<tr><td style='color:#666;padding:1px 10px 1px 0;'>%s</td>
           <td style='text-align:right;'>%s%s</td></tr>", label, value, m)
}

# section header: gray uppercase text with a line under it (e.g. DEMOGRAPHICS)
hdr <- function(txt) {
  sprintf("<tr><td colspan='2' style='padding-top:6px;font-size:10px;letter-spacing:0.05em;
           text-transform:uppercase;color:#999;border-bottom:1px solid #ddd;'>%s</td></tr>", txt)
}


# priority score classes (fixed quarters)
site_df <- site_df %>%
  mutate(score_class = cut(score,
                           breaks = c(0, 0.25, 0.50, 0.75, 1),
                           labels = c("Low", "Moderate", "High", "Very High"),
                           include.lowest = TRUE))                              # include a score of exactly 0

table(site_df$score_class)                                                      # served tracts are NA (no score)


# shorter tier labels for the map and popups
# use dplyr:: to avoid car package's recode()
site_df <- site_df %>%
  mutate(tier = dplyr::recode(tier,
                              "Tier 1: Banking Desert" = "Banking Desert",
                              "Tier 2: AFS Majority"   = "AFS Majority"))



# build the popup
map_df <- site_df %>%
  st_transform(4326) %>%                                                        # web map projection (long/lat)
  mutate(cfips = substr(GEOID, 1, 5)) %>%                                       # county FIPS from GEOID
  left_join(county_lookup, by = "cfips") %>%                                    # add county name
  mutate(
    # nearest bank: distance + travel time, labeled by how it was measured
    dist_label = case_when(
      is.infinite(access_dist_m) ~ paste0(
        format(round(dist_bank_m), big.mark = ",", trim = TRUE), " m",
        "<span style='color:#999;font-size:10px;'> straight-line</span>"),      # no route found
      use_walk  ~ paste0(
        format(round(access_dist_m), big.mark = ",", trim = TRUE), " m",
        "<span style='color:#999;font-size:10px;'> &middot; ", 
        round(tier_min), " min walk</span>"),                                   # urban: walking network
      use_drive ~ paste0(
        format(round(access_dist_m), big.mark = ",", trim = TRUE), " m",
        "<span style='color:#999;font-size:10px;'> &middot; ",
        round(tier_min), " min drive</span>"),                                  # suburban: driving network
      TRUE      ~ paste0(
        format(round(access_dist_m), big.mark = ",", trim = TRUE), " m",
        "<span style='color:#999;font-size:10px;'> straight-line</span>")       # fallback tracts: straight-line
    ),
    
    # what "nearby" means for the counts, by tract type
    count_label = case_when(
      use_walk ~ " within 15-min walk",                                         # urban: walking network
      dense    ~ " within 1.25 km",                                             # urban fallback: straight-line
      TRUE     ~ " in tract"                                                    # suburban: inside the tract
    ),
    
    # near threshold: flag tracts within 1.5 minutes of the 15-minute cutoff
    near_flag = ifelse(is.finite(tier_min) & abs(tier_min - 15) <= 1.5,
                       "<span style='color:#999;font-size:10px;'> (near threshold)</span>", ""),
    
    popup = paste0(
      "<div style='font-family:Helvetica,Arial,sans-serif;font-size:12px;color:#222;min-width:220px;'>",
      "<div style='font-weight:bold;font-size:13px;'>", county, "</div>",
      "<div style='color:#999;font-size:10px;margin-bottom:4px;'>Tract ", GEOID, "</div>",
      "<table style='border-collapse:collapse;width:100%;'>",
      
      hdr("Access"),
      row("Status", paste0(tier, near_flag)),
      row("Area Type", tools::toTitleCase(area_type)),                          # urban -> Urban
      row("Nearest Bank or Credit Union", dist_label),
      row("Banks & Credit Unions Nearby",
          paste0(banks_near, "<span style='color:#999;font-size:10px;'>", count_label, "</span>")),
      row("Pawnshops & Check Cashers Nearby",
          paste0(afs_near, "<span style='color:#999;font-size:10px;'>", count_label, "</span>")),
      
      hdr("Priority"),
      row("Score", ifelse(is.na(score), "—",                                    # served tracts: no score
                          sprintf("%s (%.2f)", score_class, score))),           # e.g. "High (0.62)"
      
      hdr("Demographics"),
      row("Population", format(total_population, big.mark = ",", trim = TRUE)),
      row("Median Income",                                                      # compared with the offical metro income
          ifelse(is.na(med_income), "—",
                 paste0("$", format(med_income, big.mark = ",", trim = TRUE),
                        "<span style='color:#999;font-size:10px;'> (metro $",
                        format(metro_income, big.mark = ",", trim = TRUE), ")</span>"))),
      row("Car-free Households",
          ifelse(is.na(pct_no_vehicle), "—", sprintf("%.1f%%", pct_no_vehicle)),
          metro_car_free),                                                      # household-weighted metro avg
      row("Poverty",           sprintf("%.1f%%", poverty_pct),        metro$poverty_pct),
      row("Bachelor's+",       sprintf("%.1f%%", bach_or_higher_pct), metro$bach_or_higher_pct),
      row("Black",             sprintf("%.1f%%", black_pct),          metro$black_pct),
      row("Hispanic",          sprintf("%.1f%%", hispanic_pct),       metro$hispanic_pct),
      row("Asian",             sprintf("%.1f%%", asian_pct),          metro$asian_pct),
      row("Foreign-born",      sprintf("%.1f%%", foreign_born_pct),   metro$foreign_born_pct),
      row("Renters",           sprintf("%.1f%%", renter_pct),         metro$renter_pct),
      row("Transit Commuters", sprintf("%.1f%%", public_transit_pct), metro$public_transit_pct),
      
      "</table></div>"
    )
  )


# select columns to keep only what the mao needs (smaller HTML file)
map_df <- map_df %>%
  select(GEOID, tier, score, score_class, top10, popup)


# split tracts into map layers
served_sf <- map_df %>% filter(tier == "Served")                                # gray background
under_sf  <- map_df %>% filter(tier != "Served")                                # deserts + AFS majority
top_sf <- map_df %>% filter(top10)                                              # top 10% outlines


# point layers, limited to the CBSA, in lat/long
banks_map_pts <- main_pts %>% st_filter(cbsa_boundary) %>% st_transform(4326)   # banks + credit unions
afs_map_pts   <- afs_pts  %>% st_filter(cbsa_boundary) %>% st_transform(4326)   # pawnshops + check cashers

cbsa_4326 <- st_transform(cbsa_boundary, 4326)                                  # CBSA outline

# mask: fade everything outside the CBSA
sf::sf_use_s2(FALSE)                                                            # use flat geometry, treat the map like a sheet of paper (by default, s2 treats the Earth as a sphere)
outer <- st_as_sfc(st_bbox(c(xmin = -180, ymin = -85, xmax = 180, ymax = 85),   # a rectangle covering the whole world
                           crs = 4326))
mask  <- st_sf(geometry = st_difference(outer, st_union(cbsa_4326)))            # the world rectangle minus the CBSA, st_sf package it as a map layer
sf::sf_use_s2(TRUE)                                                             # switch spherical geometry back on


# starting view: the five NYC boroughs
nyc_bounds <- map_df %>%
  filter(substr(GEOID, 1, 5) %in%
           c("36005", "36047", "36061", "36081", "36085"))                      # state + county FIPS



# build the maps
site_map <- maplibre(style = carto_style("positron"),                           # light gray basemap
                     bounds = nyc_bounds,                                       # zoom to NYC first
                     minZoom = 7) %>%                                           # a zoom threshold so people dont zoom out too much
  
  # outside the study area: fade + dashed CBSA outline
  add_fill_layer(id = "mask", source = mask,
                 fill_color = "#FFFFFF", fill_opacity = 0.5) %>%
  add_line_layer(id = "cbsa_outline", source = cbsa_4326,
                 line_color = "#666666", line_width = 1, line_dasharray = c(2, 2)) %>%
  
  # served tracts: light gray, clickable
  add_fill_layer(id = "Served", source = served_sf,
                 fill_color = "#E6E6E6", fill_opacity = 0.5,
                 popup = "popup") %>%
  
  # invisible layer over underserved tracts: popups and hover, handles clicking
  add_fill_layer(id = "Underserved Base", source = under_sf,
                 fill_color = "#000000", fill_opacity = 0,
                 popup = "popup") %>%
  
  # underserved tracts colored by tier (on at start)
  add_fill_layer(id = "Underserved Tracts", source = under_sf,
                 fill_color = match_expr(
                   column = "tier",
                   values = c("Banking Desert", "AFS Majority"),
                   stops  = c("#2E4A66", "#C0583A")
                 ),
                 fill_opacity = 0.8) %>%
  
  # underserved tracts colored by priority class (off at start)
  add_fill_layer(id = "Priority Score", source = under_sf,
                 fill_color = match_expr(
                   column = "score_class",
                   values = c("Low", "Moderate", "High", "Very High"),
                   stops  = c("#EBD9E8", "#C29BBE", "#8E5A8B", "#4A1F4F")
                 ),
                 fill_opacity = 0.85,
                 visibility = "none") %>%
  
  # black outline around the top 10% of each tier
  add_line_layer(id = "Top 10% Priority", source = top_sf,
                 line_color = "#111111", line_width = 1.5) %>%
  
  # banks & credit unions points (off at start)
  add_circle_layer(id = "Banks & Credit Unions", source = banks_map_pts,
                   circle_color = "#2E4A66", circle_radius = 2.5,
                   circle_stroke_color = "white", circle_stroke_width = 0.5,
                   visibility = "none") %>%
  
  # pawnshops & check cashers points (off at start)
  add_circle_layer(id = "Pawnshops & Check Cashers", source = afs_map_pts,
                   circle_color = "#C0583A", circle_radius = 2.5,
                   circle_stroke_color = "white", circle_stroke_width = 0.5,
                   visibility = "none") %>%
  
  # layer menu (top-left)
  add_layers_control(
    position = "top-left", collapsible = TRUE,
    layers = c("Underserved Tracts", "Priority Score", "Top 10% Priority",
               "Banks & Credit Unions", "Pawnshops & Check Cashers")
  ) %>%
  
  # legends: each shows only when the layer is on (layer id)
  add_categorical_legend(
    legend_title = "Physical Access",
    values = c("Banking Desert", "AFS Majority", "Served"),
    colors = c("#2E4A66", "#C0583A", "#E6E6E6"),
    position = "bottom-left",
    layer_id = "Underserved Tracts",
    width = "220px"
  ) %>%
  add_categorical_legend(
    legend_title = "Priority Score",
    values = c("Very High (0.75\u20131)", "High (0.50\u20130.75)",
               "Moderate (0.25\u20130.50)", "Low (0\u20130.25)"),
    colors = c("#4A1F4F", "#8E5A8B", "#C29BBE", "#EBD9E8"),
    position = "bottom-left",
    layer_id = "Priority Score",
    width = "220px",
    add = TRUE                                                                  # add too, don't replace the first legend
  )




# HTML, CSS, SVG (cursor ring and favicon square), JavaScript, R
# all comments must use // instead of #


# design javascript plug-in

# clear old scripts
site_map$jsHooks$render <- NULL                                                 # clear old scripts first, so reruns don't stack copies

site_map <- site_map %>%
  htmlwidgets::onRender("
function(el, x) {                                                               // el = the map's container
  const map = el.map;                                                           // the MapLibre map object
  if (!map) { console.warn('map object not found'); return; }
  if (el.dataset.customized) return;
  el.dataset.customized = 'true';

  // ---- styles ----
  // cursor: a ring drawn as SVG, encoded to be used as an image
  const ring = encodeURIComponent(
    \"<svg xmlns='http://www.w3.org/2000/svg' width='20' height='20'>\" +
    \"<circle cx='10' cy='10' r='7' fill='none' stroke='white' stroke-width='3'/>\" +
    \"<circle cx='10' cy='10' r='7' fill='none' stroke='#222' stroke-width='1.5'/></svg>\"
  );
  
  // all styling for the card, table, popup, and the buttons
  const style = document.createElement('style');
  style.innerHTML =
    '.maplibregl-canvas-container, .maplibregl-canvas { cursor: url(\"data:image/svg+xml,' +
    ring + '\") 10 10, crosshair !important; }' +
    '.label-toggle { background:#fff; border:none; border-radius:4px; padding:6px 10px;' +
    'font:12px Helvetica,Arial,sans-serif; color:#222; cursor:pointer;' +
    'box-shadow:0 0 0 2px rgba(0,0,0,0.1); }' +
    '.info-card { background:rgba(255,255,255,0.95); border-radius:6px; padding:14px 16px; width:280px;' +
    'font:12px Helvetica,Arial,sans-serif; color:#222; box-shadow:0 0 0 2px rgba(0,0,0,0.08);' +
    'max-height:80vh; overflow-y:auto; }' +
    '.ic-title { font-family:Tahoma,Verdana,sans-serif; font-weight:bold; font-size:22px; line-height:1.1;' +
    'text-transform:uppercase; color:#333; text-shadow:1px 0 0 #9a9a9a; margin-bottom:8px; }' +
    '.ic-by { color:#999; font-size:11px; margin-bottom:8px; }' +
    '.info-card summary { cursor:pointer; color:#555; font-size:11px; font-weight:bold; }' +
    '.info-card h4 { font-size:11px; text-transform:uppercase; letter-spacing:0.05em; color:#999;' +
    'margin:10px 0 4px; border-bottom:1px solid #ddd; }' +
    '.info-card p, .info-card li { margin:8px 0; font-size:11px; color:#444; line-height:1.4; text-wrap:pretty; }' +
    '.info-card ul { padding-left:16px; margin:4px 0; }' +
    '.info-card ol { padding-left:18px; margin:4px 0; }' +
    '.ic-formula { background:#f4f4f4; padding:6px 8px; border-radius:4px; font-size:11px; margin:4px 0; }' +
    '.ic-src { color:#999 !important; }' +
    '.info-card details + details { margin-top:8px; padding-top:8px; border-top:1px solid #eee; }' +
    '.reg-table { width:100%; border-collapse:collapse; font-size:10.5px; margin-top:4px; }' +
    '.reg-table th { font-weight:bold; color:#555; text-align:right; padding:3px 2px; border-bottom:1px solid #ccc; }' +
    '.reg-table td { text-align:right; padding:3px 2px; vertical-align:top; color:#222; border-bottom:1px solid #f0f0f0; }' +
    '.reg-table td.rl { text-align:left; color:#555; }' +
    '.reg-table .ci { display:block; font-size:9.5px; color:#999; }' +
    '.reg-table tr.rsec td { text-align:left; font-size:9.5px; text-transform:uppercase; letter-spacing:0.05em;' +
    'color:#999; padding-top:8px; border-bottom:1px solid #ddd; }' +
    '.reg-note { text-align:left; text-wrap:pretty; font-size:10px !important; margin-top:6px !important; }' +
        '.maplibregl-popup { z-index: 99999 !important; }' +
    '.mapboxgl-legend, .maplibregl-legend { z-index: 1 !important; }' +
    
    // phone screens (under 600px wide)
    '@media (max-width: 600px) {' +
    '  .info-card { width:170px; padding:10px 12px; max-height:45vh; }' +
    '  .ic-title { font-size:14px; margin-bottom:4px; }' +
    '  .ic-by { margin-bottom:4px; }' +
    '  .ic-hint { display:none; }' +
    '  .label-toggle { padding:4px 8px; font-size:11px; }' +
    '}';
  document.head.appendChild(style);
  
   // ---- favicon ----
   // plum square in the browser tab
  const favSvg =
    \"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 32 32'>\" +
    \"<rect x='0' y='0' width='32' height='32' rx='0' fill='#4A1F4F'/></svg>\";
  const fav = document.createElement('link');
  fav.rel = 'icon';
  fav.href = 'data:image/svg+xml,' + encodeURIComponent(favSvg);
  document.head.appendChild(fav);
  
   // all the steps that need the loaded map
   // the setup steps that must wait until the map is ready
   // if the map is fully loaded, run init()
  function init() {
    // collapse map credits into the (i) button 
    const attrib = el.querySelector('.maplibregl-ctrl-attrib');
    if (attrib) {
      attrib.classList.add('maplibregl-compact');
      attrib.classList.remove('maplibregl-compact-show');
      attrib.removeAttribute('open');
    }
    
    // ---- helper for regression table rows ----
    const r = (lab, a, aci, b, bci) =>
      '<tr><td class=\"rl\">' + lab + '</td>' +
      '<td>' + a + (aci ? '<span class=\"ci\">' + aci + '</span>' : '') + '</td>' +
      '<td>' + b + (bci ? '<span class=\"ci\">' + bci + '</span>' : '') + '</td></tr>';
    const sec = txt => '<tr class=\"rsec\"><td colspan=\"3\">' + txt + '</td></tr>';
    

    // ---- info card (top-right): collapsible sections ----
    const card = document.createElement('div');
    card.className = 'maplibregl-ctrl info-card';
    card.innerHTML =
      '<div class=\"ic-title\">Closing the<br>Banking Gap<br>in Metro New York</div>' +
      '<div class=\"ic-by\">Alex Mingtao Wang &middot; 2026</div>' +
      '<p class=\"ic-hint\" style=\"font-size:11px;color:#777;margin:20px 0 8px;text-wrap:pretty;\">Click a tract for details. Switch layers in the menu.</p>' +
            
      '<details><summary>Why This Map</summary>' +
      '<p>Physical access to banking services has been shrinking across the U.S. for over a decade.</p>' +
      '<p>Growing up in an immigrant neighborhood in Queens, I saw how much nearby banking matters. Online banking is convenient, but many of my neighbors depended on in-person help and service in their own language. I built this map to see where access is weakest, and where a new branch or better banking options could help most.</p>' +
      '<p>This study makes four contributions:</p>' +
      '<ol>' +
      '<li>It measures access by where people live and how they travel, from each tract&rsquo;s population center, along the real street network, using walking routes where many households have no car and driving routes in car-dependent areas.</li>' +
      '<li>It looks beyond banking deserts to neighborhoods where pawnshops and check cashers outnumber banks and credit unions, which is where most underserved residents live.</li>' +
      '<li>It models mainstream and alternative services side by side using negative binomial regression, revealing fewer banks and credit unions in Black neighborhoods, and more pawnshops and check cashers in Hispanic and immigrant neighborhoods.</li>' +
      '<li>It provides a tract-level interactive web map that gives policymakers, planners, and community lenders a practical starting point for advancing financial inclusion.</li>' +
      '</ol>' +
      '</details>' +
      
      
      '<details><summary>Key Findings</summary>' +
      
      '<p><b>About 2 million residents are underserved.</b> Roughly 433,000 people live in 113 banking desert tracts, and another 1.6 million live in 373 tracts where alternative services outnumber mainstream ones. Together, that is about 1 in 10 metro residents, almost all in neighborhoods where many households don&rsquo;t own a car.</p>' +

      '<p><b>Fewer mainstream services in Black, low-income, and transit-dependent neighborhoods.</b> Holding other factors equal, every 10 percentage points more Black residents is associated with 14% fewer banks and credit unions, or about 50% fewer when the share rises from 10% to 60%.</p>' +

      '<p><b>More alternative services in immigrant and Hispanic neighborhoods.</b> Every 10 points more foreign-born residents is associated with 13% more pawnshops and check cashers. Every 10 points more Hispanic residents is associated with 9% more alternative services and 6% fewer mainstream branches.</p>' +

      '<p><b>The priority areas reflect the same pattern.</b> About 324,000 people live in the highest-priority tracts. Although race is not part of the score, their residents are 62% Hispanic and 29% Black, compared with 25% and 15% across the metro.</p>' +

      '</details>' +
      
      
      '<details><summary>Methods & Data</summary>' +

      '<h4>Physical Access</h4>' +
      '<ul>' +
      '<li><b>Urban vs. Suburban:</b> access depends on how people travel, so tracts where at least 25% of households have no vehicle are urban and evaluated by walking; all others are suburban and evaluated by driving. In the metro, a histogram of car-free shares shows most car-dependent tracts fall below about 20%; the 25% threshold marks where car-light neighborhoods begin.</li>' +
      '<li><b>Banking Desert:</b> no bank or credit union inside the tract, and none within a 15-minute walk (urban tracts) or a 15-minute drive (suburban tracts), based on travel times along the street network.</li>' +
      '<li><b>AFS Majority:</b> at least two pawnshops or check cashers, outnumbering banks and credit unions within a 15-minute walk in urban tracts or within the tract boundary in suburban tracts.</li>' +
      '<li><b>Served:</b> at least one bank or credit union nearby, and they equal or outnumber pawnshops and check cashers.</li>' +
      '</ul>' +

      '<h4>Priority Score</h4>' +
      '<div class=\"ic-formula\">Score = (Poverty + AFS Nearby + Population) / 3</div>' +
      '<p>Poverty reflects households most likely to be unbanked and hit hardest by fees. AFS shows where residents already pay for costly alternatives. Population reflects how many people a new branch could serve.</p>' +
      '<p>Each factor is a percentile rank from 0 to 1, ranked within each tier. Percentile ranks put all three factors on equal footing, so no extreme value can tip the score, similar to the approach of the CDC Social Vulnerability Index.</p>' +
      '<p>Scores are grouped into four classes: Low (0&ndash;0.25), Moderate (0.25&ndash;0.50), High (0.50&ndash;0.75), and Very High (0.75&ndash;1). The top 10% of each tier is outlined:</p>' +
      '<ul>' +
      '<li><b>Banking Desert:</b> the strongest candidates for a new bank or credit union branch.</li>' +
      '<li><b>AFS Majority:</b> the strongest candidates for programs that help residents move from alternative services to mainstream banking.</li>' +
      '</ul>' +
      

      '<h4>Notes</h4>' +
      '<ul>' +
      '<li>The analysis covers residential census tracts (at least 500 residents, with enough workers and households for reliable estimates).</li>' +
      '<li>Distances and travel times are measured from each tract&rsquo;s 2020 Census center of population along the OpenStreetMap street network, using walking routes for urban tracts and driving routes for suburban tracts.</li>' +
      '<li>Median income is compared with the official metro median household income. Car-free households are weighted by number of households; all other metro figures are population-weighted across 4,761 census tracts in the New York&ndash;Newark&ndash;Jersey City CBSA (35620).</li>' +
      '<li>The map measures physical access only. It does not account for institution type, opening hours, fees, language services, or online banking.</li>' +
      '<li>The priority score is a starting point. Actual siting or program decisions would also weigh local conditions and community input.</li>' +
      '</ul>' +

      '<h4>Data</h4>' +
      '<p class=\"ic-src\">FDIC Summary of Deposits, 2023; NCUA Call Report, 2023; NYC DCWP, NYS DFS, NJ DOBI, and OpenStreetMap (accessed October 2026); U.S. Census Bureau, ACS 2019&ndash;2023 5-Year Estimates and 2020 Centers of Population.</p>' +
            '</details>' +

      '<details><summary>Negative Binomial Regression Results</summary>' +
      '<p>Incidence rate ratios per 10-point increase. Below 1 = fewer establishments, above 1 = more.</p>' +
      '<table class=\"reg-table\">' +
      '<tr><th></th><th>Banks &amp; CUs</th><th>Pawns &amp; CCs</th></tr>' +

      sec('Predictors') +
      r('Black',        '0.86***', '[0.83, 0.89]', '1.02',    '[0.96, 1.07]') +
      r('Hispanic',     '0.94**',  '[0.91, 0.98]', '1.09**',  '[1.02, 1.17]') +
      r('Asian',        '1.02',    '[0.97, 1.06]', '0.96',    '[0.88, 1.06]') +
      r('Foreign-born', '1.03',    '[0.98, 1.08]', '1.13**',  '[1.04, 1.23]') +
      r('Poverty',      '0.93*',   '[0.86, 0.99]', '1.05',    '[0.94, 1.17]') +
      r('Bachelor&rsquo;s+', '1.14***', '[1.10, 1.18]', '0.94+', '[0.87, 1.01]') +
      r('Transit Commuters', '0.81***', '[0.79, 0.84]', '1.04', '[0.99, 1.10]') +
      r('Renters',      '1.17***', '[1.14, 1.20]', '1.16***', '[1.10, 1.23]') +
      r('Median Age (per 10 yrs)', '1.23***', '[1.14, 1.32]', '0.89', '[0.76, 1.04]') +
      r('Log Population', '1.95***', '[1.75, 2.17]', '1.62***', '[1.32, 1.99]') +

      sec('Model fit') +
      r('N (Tracts)',          '4,761',  '', '4,761',  '') +
      r('AIC',                 '11904.6','', '3489.7', '') +
      r('Theta',               '0.858',  '', '0.832',  '') +
      r('Deviance Explained',  '18.3%',  '', '19.5%',  '') +
      r('DHARMa Dispersion',   '0.94',   '(p = 0.36)', '0.99', '(p = 0.95)') +
      r('Zero-inflation Ratio','1.01',   '(p = 0.59)', '1.00', '(p = 1.00)') +
      r('Max VIF',             '3.58',   '(Foreign-born)', '3.56', '(Hispanic)') +
      r('Moran&rsquo;s I (Residuals)', '0.002', '(p = 0.30)', '&minus;0.002', '(p = 0.61)') +
      '</table>' +
      '<p class=\"ic-src reg-note\">+ p &lt; 0.1, * p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001</p>' +
      '<p class=\"ic-src reg-note\">95% CIs in brackets. White (%) is the reference category.</p>' +
      '<p class=\"ic-src reg-note\">VIF below 5 indicates no concerning multicollinearity.</p>' +
      '<p class=\"ic-src reg-note\">Moran&rsquo;s I uses randomized quantile residuals (DHARMa); values near 0 with p &gt; 0.05 indicate no remaining spatial autocorrelation.</p>' +
      '</details>';
    el.querySelector('.maplibregl-ctrl-top-right').appendChild(card);


    // ---- label toggle: hide basemap place names at the start; button switches them on/off ----
    const labelIds = map.getStyle().layers
      .filter(l => l.type === 'symbol').map(l => l.id);
    let labelsOn = false;
    labelIds.forEach(id => map.setLayoutProperty(id, 'visibility', 'none'));
    const btn = document.createElement('button');
    btn.className = 'label-toggle';
    btn.textContent = 'Labels: Off';
    btn.onclick = () => {
      labelsOn = !labelsOn;
      labelIds.forEach(id => map.setLayoutProperty(id, 'visibility', labelsOn ? 'visible' : 'none'));
      btn.textContent = labelsOn ? 'Labels: On' : 'Labels: Off';
    };
    const ctrl = document.createElement('div');
    ctrl.className = 'maplibregl-ctrl';
    ctrl.appendChild(btn);
    el.querySelector('.maplibregl-ctrl-top-right').appendChild(ctrl);

    // ---- hover outline: drow a gray border around the tract under the cursor ----
    map.addSource('hover-src', { type: 'geojson', data: { type: 'FeatureCollection', features: [] } });
    map.addLayer({ id: 'hover-line', type: 'line', source: 'hover-src',
                   paint: { 'line-color': '#666', 'line-width': 1.2 } });

    const tractLayers = ['Served', 'Underserved Base'];
    map.on('mousemove', e => {
      const layers = tractLayers.filter(id => map.getLayer(id) &&
                       map.getLayoutProperty(id, 'visibility') !== 'none');
      const f = map.queryRenderedFeatures(e.point, { layers: layers })[0];
      map.getSource('hover-src').setData(f
        ? { type: 'Feature', geometry: f.geometry, properties: {} }
        : { type: 'FeatureCollection', features: [] });
    });
    map.on('mouseout', () =>
      map.getSource('hover-src').setData({ type: 'FeatureCollection', features: [] }));
      
    // ---- link Priority Score and Underserved Tracts toggles ----
    // only one can show at a time
    function findToggle(name) {
      const nodes = Array.from(el.querySelectorAll('.maplibregl-ctrl-top-left *'))
        .filter(n => n.children.length === 0 && n.textContent.trim() === name);
      return nodes.length ? (nodes[0].closest('a, label, button') || nodes[0]) : null;
    }
    const isOn = id => map.getLayoutProperty(id, 'visibility') !== 'none';

    const scoreBtn = findToggle('Priority Score');
    const underBtn = findToggle('Underserved Tracts');

    if (scoreBtn && underBtn) {
      scoreBtn.addEventListener('click', () => setTimeout(() => {
        // score on -> underserved off; score off -> underserved on
        if (isOn('Priority Score') === isOn('Underserved Tracts')) underBtn.click();
      }, 50));

      underBtn.addEventListener('click', () => setTimeout(() => {
        // underserved on -> score off (keeps only one fill layer at a time)
        if (isOn('Underserved Tracts') && isOn('Priority Score')) scoreBtn.click();
      }, 50));
    } else {
      console.warn('layer toggles not found');
    }
    
  }
  
   // run init now if the mao is already loaded, otherwise wait for it
  if (map.loaded()) init(); else map.on('load', init);
}
")

site_map                                                                        # preview

# save as one self-contained HTML file for the website
htmlwidgets::saveWidget(site_map, "site_selection_map.html",
                        selfcontained = TRUE, title = "Closing the Banking Gap in Metro New York")
