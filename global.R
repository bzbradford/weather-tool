#-- global.R --#

# dev ----
# shiny::devmode(TRUE)
# renv::snapshot()


suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(sf)
  # library(httr2) # requests
  # library(zoo) # rollmean
  # library(lutz) # timezones

  library(shiny)
  # library(bslib)
  library(shinydashboard)
  # library(shinythemes)
  library(shinyWidgets)
  library(shinyBS)
  library(shinyjs)
  library(htmltools)

  library(leaflet)
  library(leaflet.extras)
  library(DT)
  library(plotly)
  library(gt)
})


# new_site <- function(lat, lng, name, id, )

# new_site <- setClass("site", slots = c(
#   name = "character",
#   lat = "numeric",
#   lng = "numeric"))
#
# new_site(name = 2)

sites_template <- tibble(
  id = integer(),
  name = character(),
  lat = numeric(),
  lng = numeric(),
  group = character(),
  icon = character()
)

ibm_cols <- read_csv("data/ibm_cols.csv", show_col_types = F)

# new_site <- function(lat, lng, name, )


# Functions ---------------------------------------------------------------

#' parse lat/lng coordinates from string
#' @param str input string containing coordinates to parse in form "lat, lng"
#' @return named list { lat: numeric, lng: numeric }

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


#' returns TRUE if location is within service boundary
#' @param loc list of type { lat: numeric, lng: numeric }
#' @return boolean

validate_loc <- function(loc, bounds = OPTS$map_bounds_us) {
  validate_ll(loc$lat, loc$lng)
}

validate_ll <- function(lat, lng, bounds = OPTS$map_bounds_us) {
  if (!isTruthy(lat) | !isTruthy(lng)) return(F)
  between(lat, bounds$lat1, bounds$lat2) &
    between(lng, bounds$lng1, bounds$lng2)
}

# finds center and bounds of IBM grid cell
# grid dimensions are 1/45.5 degrees
# only provides an approximate grid location
find_grid <- function(lat, lng) {
  d <- 22.75
  grd <- list(
    lat1 = floor(lat * d) / d,
    lat2 = ceiling(lat * d) / d,
    lng1 = floor(lng * d) / d,
    lng2 = ceiling(lng * d) / d
  )
  grd$latc <- mean(c(grd$lat1, grd$lat2))
  grd$lngc <- mean(c(grd$lng1, grd$lng2))
  grd
}


load_sites <- function(df, cols = OPTS$site_cols) {
  df <- df %>%
    clean_names() %>%
    select(any_of(cols)) %>%
    drop_na()
  req(c("name", "lat", "lng") %in% names(df))
  df <- df %>%
    filter(validate_ll(lat, lng)) %>%
    mutate(id = row_number(), .before = 1) %>%
    mutate(group = "sites")
  req(nrow(df) > 0)
  df
}


#' breaks up longer time periods into 1000 hour chunks
#' @param start_date date or YYYY-MM-DD string
#' @param end_date date or YYYY-MM-DD string
#' @return list of two-element formatted datetime strings

ibm_chunks <- function(start_date, end_date) {
  start_dttm <- as_datetime(start_date)
  end_dttm <- as_datetime(end_date) + hours(23)
  chunks <- seq(start_dttm, end_dttm, by = as.difftime(hours(999)))
  chunks <- lapply(1:length(chunks), function(i) {
    c(chunks[i], ifelse(i < length(chunks), chunks[i + 1], end_dttm))
  })
  lapply(chunks, function(chunk) format(chunk, "%Y-%m-%dT%H:%M:%S%z"))
}

# ibm_chunks("2024-1-1", Sys.Date())


# fetch hourly weather data from IBM
#' @param lat latitude of point
#' @param lng longitude of point
#' @param start_date date or date string
#' @param end_date date or date string
#' @return hourly weather data response as tibble
#' documentation: https://docs.google.com/document/d/13HTLgJDpsb39deFzk_YCQ5GoGoZCO_cRYzIxbwvgJLI/edit?tab=t.0
get_ibm <- function(lat, lng, start_date, end_date) {
  require(httr2)

  stime <- Sys.time()
  chunks <- ibm_chunks(start_date, end_date)
  reqs <- lapply(chunks, function(dates) {
    request(OPTS$ibm_endpoint) %>%
      req_url_query(
        format = "json",
        geocode = str_glue("{lat},{lng}"),
        startDateTime = dates[1],
        endDateTime = dates[2],
        units = "s",
        apiKey = OPTS$ibm_key
      )
  })
  resps <- req_perform_parallel(reqs, on_error = "continue", progress = F)
  weather <- lapply(resps, function(resp) {
    if (resp$status == 200)
      as_tibble(resp_body_json(resp, simplifyVector = T))
    else
      tibble()
  }) %>% bind_rows()
  msg <- "weather for {lat}, {lng} from {start_date} to {end_date} in {Sys.time() - stime}"
  if (nrow(weather) > 0) {
    message("OK ==> Got ", msg)
  } else {
    message("FAIL ==> Could not get ", msg)
  }
  weather
}


ll_to_grid <- function(lat, lon, d = OPTS$grid_dim) {
  m <- list(rbind(
    c(lon - d, lat + d),
    c(lon + d, lat + d),
    c(lon + d, lat - d),
    c(lon - d, lat - d),
    c(lon - d, lat + d)
  ))
  st_sfc(st_polygon(m), crs = 4326)
}


build_grids <- function(ibm_hourly) {
  req(nrow(ibm_hourly) > 0)

  ibm_hourly %>%
    distinct(
      grid_id = gridpointId,
      grid_lat = latitude,
      grid_lng = longitude
    ) %>%
    mutate(
      tz = lutz::tz_lookup_coords(grid_lat, grid_lng, warn = F)
    )
}

# grid_lookup <- function(lat, lng, grids) {
#   print(grids)
#   id <- grids %>%
#     filter(between(lat, lat1, lat2) & between(lng, lng1, lng2)) %>%
#     pull(grid_id)
#   print(id)
#   if (length(id) > 0) id else NA
# }

build_hourly <- function(ibm_hourly, grids = build_grids(ibm_hourly)) {
  req(nrow(ibm_hourly) > 0)

  ibm_hourly %>%
    select(
      # lat = requestedLatitude,
      # lng = requestedLongitude,
      grid_id = gridpointId,
      datetime_utc = validTimeUtc,
      # C
      temp = temperature,
      dewpoint = temperatureDewPoint,
      # %
      rh = relativeHumidity,
      # mm
      precip = precip1Hour,
      # m/s
      windspeed = windSpeed
    ) %>%
    left_join(grids, join_by(grid_id)) %>%
    relocate(grid_lat, grid_lng, .after = grid_id) %>%
    relocate(tz, .after = datetime_utc) %>%
    mutate(across(datetime_utc, ~parse_date_time(.x, "YmdHMSz"))) %>%
    mutate(
      datetime_local = with_tz(datetime_utc, first(tz)),
      # date, local time
      date = as_date(datetime_local),
      # hour of day, local
      hour = hour(datetime_local),
      # night is between 20:00 and 6:00
      night = !between(hour, 7, 19),
      # date starting at nightfall the day before
      date_since_night = as_date(datetime_local + hours(4)),
      .after = datetime_utc,
      .by = tz
    ) %>%
    mutate(
      rh_over_80 = rh >= 80,
      rh_over_90 = rh >= 90,
      .after = rh
    ) %>%
    arrange(grid_lat, grid_lng, datetime_utc)
}

# this summarizes based on the "date since night" eg since 8 pm the day before
build_daily <- function(hourly) {
  by_date <- hourly %>%
    summarize(
      across(
        c(temp, dewpoint, rh, windspeed),
        c(min = min, mean = mean, max = max)
      ),
      precip_total = sum(precip),
      precip_max_rate = max(precip),
      hours_rh_over_80 = sum(rh_over_80),
      hours_rh_over_90 = sum(rh_over_90),
      hours_missing = 24 - n(),
      .by = c(grid_id, date)
    ) %>%
    filter(hours_missing < 6)
  by_night <- hourly %>%
    summarize(
      hours_rh_over_80_night = sum(night & rh_over_80),
      hours_rh_over_90_night = sum(night & rh_over_90),
      .by = c(grid_id, date_since_night)
    )
  by_date %>%
    left_join(by_night, join_by(grid_id, date == date_since_night)) %>%
    add_moving_averages()
}

roll_mean <- function(vec, width) {
  zoo::rollapply(vec, width, \(x) mean(x, na.rm = T), fill = NA, partial = T)
}

add_moving_averages <- function(daily) {
  daily %>%
    mutate(
      temp_min_30day = roll_mean(temp_min, 30),
      temp_mean_30day = roll_mean(temp_mean, 30),
      temp_max_30day = roll_mean(temp_max, 30),
      temp_min_21day = roll_mean(temp_min, 21),
      dewpoint_min_21day = roll_mean(dewpoint_min, 21),
      rh_max_30day = roll_mean(rh_max, 30),
      hours_rh_over_90_night_14day = roll_mean(hours_rh_over_90_night, 14),
      hours_rh_over_80_30day = roll_mean(hours_rh_over_80, 30),
      windspeed_max_30day = roll_mean(windspeed_max, 30)
    )
}



# Settings ----------------------------------------------------------------

OPTS <- lst(
  ibm_endpoint = "https://api.weather.com/v3/wx/hod/r1/direct",
  ibm_key = Sys.getenv("ibm_key"),
  google_key = Sys.getenv("google_places_key"),

  # radius of the grid cell
  grid_dim = 1/45.5,

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
  ),
  map_click_zoom = 10,

  # site handling
  site_cols = c(
    name = "name",
    name = "location",
    lat = "latitude",
    lng = "longitude",
    lng = "long"
  )
)

