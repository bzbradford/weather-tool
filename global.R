#-- global.R --#

# dev ----
# shiny::devmode(TRUE)
# renv::snapshot()


suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(sf)

  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(shinyBS)
  library(shinyjs)
  library(htmltools)

  library(leaflet)
  library(leaflet.extras)
  library(DT)
  library(plotly)
  # library(gt)
})



# Functions ---------------------------------------------------------------

#* @param str input string containing coordinates to parse in form "lat, lng"
parse_coords <- function(str) {
  str <- gsub("[ °NW]", "", str)
  parts <- str_split_1(str, ",")
  if (length(parts) != 2) stop("Invalid coordinate format.")
  coords <- suppressWarnings(list(
    lat = as.numeric(parts[1]),
    lng = as.numeric(parts[2])
  ))
  if (any(sapply(coords, is.na))) stop("Failed to parse coordinates.")
  coords
}

#* @param loc list of type { lat: numeric, lng: numeric }
validate_loc <- function(loc) {
  if (!isTruthy(loc$lat) | !isTruthy(loc$lng)) return(FALSE)
  bounds <- OPTS$map_bounds_us
  between(loc$lat, bounds$lat1, bounds$lat2) &
    between(loc$lng, bounds$lng1, bounds$lng2)
}


# Settings ----------------------------------------------------------------

OPTS <- lst(
  ibm_key = Sys.getenv("ibm_key"),
  google_key = Sys.getenv("google_places_key"),

  # map
  map_bounds_wi = list(
    lat1 = 42.4,
    lat2 = 47.1,
    lng1 = -93.0,
    lng2 = -86.8
  ),
  map_bounds_us = list(
    lat1 = 24.5,
    lat2 = 49.0,
    lng1 = -125.0,
    lng2 = -66.9
  ),
  map_tiles = list(
    "ESRI Topo" = providers$Esri.WorldTopoMap,
    "Satellite" = providers$Esri.WorldImagery,
    "OpenStreetMap" = providers$OpenStreetMap,
    "Grey Canvas" = providers$CartoDB.Positron
  ),
  map_layers = list(
    grid = "Data grid",
    counties = "Counties/Regions"
  )
)




# Testing -----------------------------------------------------------------



test_sites <- read_csv("example-sites.csv", show_col_types = F) %>%
  select(
    name = Location,
    lat = Latitude,
    lng = Longitude
  ) %>%
  mutate(label = sprintf("%s (%.2f, %.2f)", name, lat, lng))

# test_sites_sf <- test_sites %>%
#   st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
#
# test_bbox <- st_bbox(test_sites_sf) %>% as.list()
# names(test_bbox) <- c("lng1", "lat1", "lng2", "lat2")
