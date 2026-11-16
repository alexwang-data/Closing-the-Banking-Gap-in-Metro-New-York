## Setup: Libraries and API Keys________________________________________________


if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("tidycensus", quietly = TRUE)) install.packages("tidycensus")
if (!requireNamespace("tidygeocoder", quietly = TRUE)) install.packages("tidygeocoder")
if (!requireNamespace("tigris", quietly = TRUE)) install.packages("tigris")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("spdep", quietly = TRUE)) install.packages("spdep")
if (!requireNamespace("spatialreg", quietly = TRUE)) install.packages("spatialreg")
if (!requireNamespace("ggpointdensity", quietly = TRUE)) install.packages("ggpointdensity")
if (!requireNamespace("mapgl", quietly = TRUE)) install.packages("mapgl")
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
if (!requireNamespace("DHARMa", quietly = TRUE)) install.packages("DHARMa")
if (!requireNamespace("olsrr", quietly = TRUE)) install.packages("olsrr")
if (!requireNamespace("psych", quietly = TRUE)) install.packages("psych")
if (!requireNamespace("classInt", quietly = TRUE)) install.packages("classInt")
if (!requireNamespace("socratadata", quietly = TRUE)) install.packages("socratadata")
if (!requireNamespace("osmdata", quietly = TRUE)) install.packages("osmdata")
if (!requireNamespace("osmextract", quietly = TRUE)) install.packages("osmextract")
if (!requireNamespace("dodgr", quietly = TRUE)) install.packages("dodgr")
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")
if (!requireNamespace("gt", quietly = TRUE)) install.packages("gt")
if (!requireNamespace("modelsummary", quietly = TRUE)) install.packages("modelsummary")


library(tidyverse)        # for data manipulation and visualization
library(tidycensus)       # for Census API (ACS)
library(tidygeocoder)     # for geocoding addresses
library(tigris)           # for CBSA metro region boundaries
library(sf)               # for spatial analysis
library(spdep)            # for autocorrelation tests
library(spatialreg)       # for spatial lag regression
library(ggpointdensity)   # for Moran's I scatterplot
library(mapgl)            # for building interactive web map
library(car)              # for evaluating model assumptions and controlling predictors with avplots
library(DHARMa)           # for DHARMa simulated residuals (negative binomial model)
library(olsrr)            # for stepwise modeling (check predictive variables)
library(psych)            # for descriptive statistics
library(classInt)         # for natural jenks 
library(socratadata)      # for Socrata API (NYC Open Data)
library(osmdata)          # for OSM Overpass API (pawnshops and checkcashers in NYS)
library(osmextract)       # for downloading OSM data (network distance)
library(dodgr)            # for measuring distances along the streets (network distance)
library(httr)             # for sending network requests to FDIC API
library(jsonlite)         # for translating FDIC json into a data frame
library(patchwork)        # for plot composition
library(gt)               # for table composition
library(modelsummary)     # for model summary


# define Census API Key
census_api_key("INSERT API KEY HERE")


# define NYC Open Data API V3 Key
Sys.setenv(
  soc_api_key_id = "INSERT API KEY HERE",
  soc_api_key_secret = "INSERT API KEY HERE"
)
