#-- global.R --#

# dev ----
# shiny::devmode(TRUE)
# renv::snapshot()


suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(sf)
  library(fst)
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


# Functions ---------------------------------------------------------------

# ternary operator
# `?` <- function(x, y) {
#   eval(
#     sapply(
#       strsplit(deparse(substitute(y)), ":"),
#       function(e) parse(text = e)
#     )[[2 - shiny::isTruthy(x)]]
#   )
# }

#' message and print an object to the console for testing
echo <- function(x) {
  message(deparse(substitute(x)))
  print(x)
}

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

#' finds center and bounds of IBM grid cell
#' grid dimensions are 1/45.5 degrees
#' only provides an approximate grid location
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


#' breaks up longer time periods into 1000 hour chunks
#' @param start_date date or YYYY-MM-DD string
#' @param end_date date or YYYY-MM-DD string
#' @return list of two-element formatted datetime strings

ibm_chunks <- function(start_date, end_date, tz = "UTC") {
  start_dttm <- as_datetime(start_date, tz = tz)
  end_dttm <- as_datetime(end_date, tz = tz) + hours(23)
  chunks <- seq(start_dttm, end_dttm, by = as.difftime(hours(999)))
  chunks <- lapply(1:length(chunks), function(i) {
    c(chunks[i], ifelse(i < length(chunks), chunks[i + 1], end_dttm))
  })
  lapply(chunks, function(chunk) format(chunk, "%Y-%m-%dT%H:%M:%S%z"))
}

# ibm_chunks("2024-1-1", Sys.Date())


#' Fetch hourly weather data from IBM
#' @param lat latitude of point
#' @param lng longitude of point
#' @param start_date date or date string
#' @param end_date date or date string
#' @return hourly weather data response as tibble
#' documentation: https://docs.google.com/document/d/13HTLgJDpsb39deFzk_YCQ5GoGoZCO_cRYzIxbwvgJLI/edit?tab=t.0
get_ibm <- function(lat, lng, start_date, end_date) {
  require(httr2)

  stime <- Sys.time()
  tz <- lutz::tz_lookup_coords(lat, lng, warn = F)
  chunks <- ibm_chunks(start_date, end_date, tz)
  reqs <- lapply(chunks, function(dates) {
    request(OPTS$ibm_endpoint) %>%
      req_url_query(
        format = "json",
        geocode = str_glue("{lat},{lng}"),
        startDateTime = dates[1],
        endDateTime = dates[2],
        units = "m",
        apiKey = OPTS$ibm_key
      )
  })
  resps <- req_perform_parallel(reqs, on_error = "continue", progress = F)
  responses <- lapply(resps, function(resp) {
    if (resp$status == 200) {
      as_tibble(resp_body_json(resp, simplifyVector = T))
    } else {
      message("Received status ", resp$status, " with message ", resp$message)
      tibble()
    }
  })
  weather <- bind_rows(responses)
  msg <- str_glue("weather for {lat}, {lng} from {start_date} to {end_date} in {Sys.time() - stime}")
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

build_grids <- function(hourly) {
  hourly %>%
    # distinct(grid_id, grid_lat, grid_lng) %>%
    summarize(
      date_min = min(date),
      date_max = max(date),
      days_expected = as.integer(max(date) - min(date)) + 1,
      days_actual = n_distinct(date),
      days_missing = days_expected - days_actual,
      days_missing_pct = 100 * days_missing / days_actual,
      hours_expected = days_expected * 24,
      hours_actual = n(),
      hours_missing = hours_expected - hours_actual,
      hours_missing_pct = 100 * hours_missing / hours_actual,
      .by = c(grid_id, grid_lat, grid_lng)
    ) %>%
    rowwise() %>%
    mutate(geometry = ll_to_grid(grid_lat, grid_lng)) %>%
    ungroup() %>%
    st_set_geometry("geometry")
}

build_grid_summary <- function(hourly) {
  hourly %>%
    summarize(
      date_min = min(date),
      date_max = max(date),
      days_expected = as.integer(max(date) - min(date)) + 1,
      days_actual = n_distinct(date),
      days_missing = days_expected - days_actual,
      days_missing_pct = 100 * days_missing / days_actual,
      hours_expected = days_expected * 24,
      hours_actual = n(),
      hours_missing = hours_expected - hours_actual,
      hours_missing_pct = 100 * hours_missing / hours_actual,
      .by = grid_id
    )
}

clean_ibm <- function(ibm_response) {
  ibm_response %>%
    select(-OPTS$ibm_ignore_cols) %>%
    select(
      grid_id = gridpointId,
      grid_lat = latitude,
      grid_lng = longitude,
      datetime_utc = validTimeUtc,
      everything()
    ) %>%
    clean_names() %>%
    mutate(across(datetime_utc, ~parse_date_time(.x, "YmdHMSz"))) %>%
    mutate(time_zone = lutz::tz_lookup_coords(grid_lat, grid_lng, warn = F), .after = datetime_utc) %>%
    mutate(datetime_local = with_tz(datetime_utc, first(time_zone)), .by = time_zone, .after = time_zone) %>%
    mutate(date = as_date(datetime_local), .after = datetime_local)
}

# cleaned hourly weather from IBM response
build_hourly <- function(ibm_hourly) {
  ibm_hourly %>%
    select(
      grid_id,
      grid_lat,
      grid_lng,
      datetime_utc,
      time_zone,
      datetime_local,
      date,
      temperature,                       # degrees C
      dew_point = temperature_dew_point, # degrees C
      rh = relative_humidity,            # %
      precip = precip1hour,              # mm
      wind_speed = wind_speed            # km/hr, converted to m/s by dividing 3.6
    ) %>%
    mutate(across(wind_speed, ~.x / 3.6)) %>%
    mutate(
      yday = yday(date),
      year = year(date),
      month = month(date),
      month_name = month.name[month],
      day = day(date),
      hour = hour(datetime_local),
      night = !between(hour, 7, 19), # night is between 20:00 and 6:00
      date_since_night = as_date(datetime_local + hours(4)),
      .after = date,
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
  lat_lng <- hourly %>%
    distinct(grid_id, grid_lat, grid_lng)
  by_date <- hourly %>%
    summarize(
      across(
        c(temperature, dew_point, rh, wind_speed),
        c(min = min, mean = mean, max = max)
      ),
      precip_total = sum(precip),
      precip_max_rate = max(precip),
      hours_rh80 = sum(rh_over_80),
      hours_rh90 = sum(rh_over_90),
      hours_missing = 24 - n(),
      .by = c(grid_id, date)
    ) %>%
    filter(hours_missing < 6)
  by_night <- hourly %>%
    summarize(
      hours_rh80_night = sum(night & rh_over_80),
      hours_rh90_night = sum(night & rh_over_90),
      .by = c(grid_id, date_since_night)
    )
  by_date %>%
    left_join(by_night, join_by(grid_id, date == date_since_night)) %>%
    left_join(lat_lng, join_by(grid_id)) %>%
    relocate(grid_lat, grid_lng, .after = grid_id) %>%
    arrange(grid_id, date) %>%
    add_moving_averages() %>%
    add_disease_probs()
}

roll_mean <- function(vec, width) {
  zoo::rollapply(vec, width, \(x) mean(x, na.rm = T), fill = NA, partial = T)
}

add_moving_averages <- function(daily) {
  daily %>%
    mutate(
      temp_min_30ma = roll_mean(temperature_min, 30),
      temp_mean_30ma = roll_mean(temperature_mean, 30),
      temp_max_30ma = roll_mean(temperature_max, 30),
      temp_min_21ma = roll_mean(temperature_min, 21),
      dew_point_min_30ma = roll_mean(dew_point_min, 30),
      rh_max_30ma = roll_mean(rh_max, 30),
      hours_rh90_night_14ma = roll_mean(hours_rh90_night, 14),
      hours_rh80_30ma = roll_mean(hours_rh80, 30),
      wind_speed_max_30ma = roll_mean(wind_speed_max, 30)
    )
}

add_disease_probs <- function(daily) {
  daily %>%
    mutate(
      sporecaster_dry_prob = sporecaster_dry(temp_max_30ma, wind_speed_max_30ma),
      sporecaster_irrig_30_prob = sporecaster_irrig(temp_max_30ma, rh_max_30ma, "30"),
      sporecaster_irrig_15_prob = sporecaster_irrig(temp_max_30ma, rh_max_30ma, "15"),
      frogeye_leaf_spot_prob = predict_fls(temp_max_30ma, hours_rh80_30ma),
      gray_leaf_spot_prob = predict_gls(temp_min_21ma, dew_point_min_30ma),
      tarspot_prob = predict_tarspot(temp_mean_30ma, rh_max_30ma, hours_rh90_night_14ma)
    )
}


# Disease models ----------------------------------------------------------

# Logistic function to convert logit to probability
logistic <- function(logit) {
  exp(logit) / (1 + exp(logit))
}

#' Apothecial sporecaster, dryland model
#' @param max_temp_30ma 30-day moving average of daily maximum temperature, Celsius
#' @param max_wind_30ma 30-day moving average of daily maximum wind speed, m/s
sporecaster_dry <- function(temp_max_30ma, wind_max_30ma) {
  mu <- -0.47 * temp_max_30ma - 1.01 * wind_max_30ma + 16.65
  logistic(mu)
}

#' Apothecial sporecaster, irrigated model
#' @param temp_max_30ma Maximum daily temperature, 30-day moving average, Celsius
#' @param rh_max_30ma Maximum daily relative humidity, 30-day moving average, 0-100%
#' @param spacing Row spacing, either "15" or "30", inches
sporecaster_irrig <- function(temp_max_30ma, rh_max_30ma, spacing = c("15", "30")) {
  spacing <- match.arg(spacing)
  mu <- -2.38 * (spacing == "30") + 0.65 * temp_max_30ma + 0.38 * rh_max_30ma - 52.65
  logistic(mu)
}

#' Frogeye leaf spot model
#' use when growth stage within R1-R5 and no fungicide in last 14 days
#' @param temp_max_30ma Maximum daily temperature, 30-day moving average, Celsius
#' @param hours_rh80_30ma Daily hours RH > 80%, 30-day moving average, 0-24 hours
predict_fls <- function(temp_max_30ma, hours_rh80_30ma) {
  mu <- -5.92485 + 0.12208 * temp_max_30ma + 0.17326 * hours_rh80_30ma
  logistic(mu)
}

#' Gray leaf spot model
#' use when growth stage V10-R3 and no fungicide in last 14 days
#' @param temp_min_21ma Minimum daily temperature, 21-day moving average, Celsius
#' @param dew_point_min_30ma Minimum dew point temperature, 30-day moving average, Celsius
predict_gls <- function(temp_min_21ma, dew_point_min_30ma) {
  mu <- -2.9467 - 0.03729 * temp_min_21ma + 0.6534 * dew_point_min_30ma
  logistic(mu)
}

#' Tarspot model
#' @param temp_mean_30ma Mean daily temperature, 30-day moving average, Celsius
#' @param rh_max_30ma Maximum daily relative humidity, 30-day moving average, 0-100%
#' @param hours_rh90_night_14ma Nighttime hours RH > 90%, 14-day moving average, 0-24 hours
predict_tarspot <- function(temp_mean_30ma, rh_max_30ma, hours_rh90_night_14ma) {
  mu1 <- 32.06987 - 0.89471 * temp_mean_30ma - 0.14373 * rh_max_30ma
  mu2 <- 20.35950 - 0.91093 * temp_mean_30ma - 0.29240 * hours_rh90_night_14ma
  (logistic(mu1) + logistic(mu2)) / 2
}



# Load data ----

sites_template <- tibble(
  id = integer(),
  name = character(),
  lat = numeric(),
  lng = numeric(),
  temp = logical()
)

ibm_cols <- read_csv("data/ibm_cols.csv", show_col_types = F)

saved_weather <- if (file.exists("saved_weather.fst")) {
  as_tibble(read_fst("saved_weather.fst"))
}



# Settings ----

OPTS <- lst(
  ibm_endpoint = "https://api.weather.com/v3/wx/hod/r1/direct",
  ibm_key = Sys.getenv("ibm_key"),
  google_key = Sys.getenv("google_places_key"),

  # radius of the IBM grid cell
  grid_dim = 1/45.5,
  ibm_ignore_cols = c(
    "requestedLatitude",
    "requestedLongitude",
    "iconCode",
    "iconCodeExtended",
    "drivingDifficultyIndex"
  ),

  # dates
  earliest_date = make_date(2024, 1, 1),
  default_start_date = today() - 30,

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
  # map_layers = list(
  #   grid = "Data grid",
  #   counties = "Counties/Regions"
  # ),
  map_click_zoom = 10,

  # allowable names for site loading
  site_cols = c(
    name = "name",
    name = "location",
    lat = "latitude",
    lng = "longitude",
    lng = "long"
  ),

  # data types
  data_type_choices = list(
    "Hourly (select)" = "hourly",
    "Hourly (all)" = "ibm",
    "Daily" = "daily"
  ),

  # plotting
  plot_ignore_cols = c("grid_id", "grid_lat", "grid_lng", "datetime_utc", "time_zone", "datetime_local", "date", "yday", "year", "month", "month_name", "day", "hour", "night", "date_since_night"),
  plot_cols = list(
    "ibm" = c("temperature", "temperature_change24hour", "temperature_max24hour", "temperature_min24hour", "temperature_dew_point", "temperature_feels_like", "precip1hour", "precip6hour", "precip24hour", "precip2day", "precip3day", "precip7day", "precip_mtd", "precip_ytd", "pressure_change", "pressure_mean_sea_level", "relative_humidity", "snow1hour", "snow6hour", "snow24hour", "snow2day", "snow3day", "snow7day", "snow_mtd", "snow_season", "snow_ytd","uv_index", "visibility", "wind_direction", "wind_gust", "wind_speed"),
    "hourly" = c("temperature", "dew_point", "rh", "precip", "wind_speed"),
    "daily" = c("temperature_min","temperature_mean","temperature_max","dew_point_min","dew_point_mean","dew_point_max","rh_min","rh_mean","rh_max","wind_speed_min","wind_speed_mean","wind_speed_max","precip_total","precip_max_rate","hours_rh80","hours_rh90","hours_missing","hours_rh80_night","hours_rh90_night","temp_min_30ma","temp_mean_30ma","temp_max_30ma","temp_min_21ma","dew_point_min_30ma","rh_max_30ma","hours_rh90_night_14ma","hours_rh80_30ma","wind_speed_max_30ma","sporecaster_dry_prob","sporecaster_irrig_30_prob","sporecaster_irrig_15_prob","frogeye_leaf_spot_prob","gray_leaf_spot_prob","tarspot_prob")
  )
)


# Data structures ----

cat_names <- function(df) {
  message(deparse(substitute(df)))
  cat("c(")
  cat(paste(paste0("\"", names(df), "\""), collapse = ","))
  cat(")\n")
}

get_specs <- function() {
  ibm <- get_ibm(45, -89, today() - 1, today())
  cat_names(ibm)

  ibm_clean <- clean_ibm(ibm)
  cat_names(ibm_clean)

  hourly <- build_hourly(ibm_clean)
  cat_names(hourly)

  daily <- build_daily(hourly)
  cat_names(daily)
}

# get_specs()


