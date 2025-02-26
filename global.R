#-- global.R --#

# dev ----
# shiny::devmode(TRUE)
# renv::snapshot()
# renv::update()
# renv::clean()


suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor) # name cleaning
  library(sf) # GIS
  library(fst) # file storage
  library(httr2) # requests
  library(markdown)

  library(shiny)
  library(shinythemes)
  library(shinyWidgets)
  # library(shinyBS)
  library(shinyjs)
  library(htmltools)

  library(leaflet)
  library(leaflet.extras)
  library(plotly)
  # library(gt)
})


# Functions --------------------------------------------------------------------

## Utility ----

# ternary operator
# `?` <- function(x, y) {
#   eval(
#     sapply(
#       strsplit(deparse(substitute(y)), ":"),
#       function(e) parse(text = e)
#     )[[2 - shiny::isTruthy(x)]]
#   )
# }

# message and print an object to the console for testing
echo <- function(x) {
  message(deparse(substitute(x)))
  print(x)
}

# swaps names and values in a list or vector
invert <- function(x) {
  y <- as(names(x), class(x))
  names(y) <- x
  y
}

# return the first truthy argument
first_truthy <- function(...) {
  for (arg in list(...)) if (shiny::isTruthy(arg)) return(arg)
  NULL
}


# NA-safe summary functions
calc_sum <- function(x) {
  if (all(is.na(x))) return(NA)
  sum(x, na.rm = TRUE)
}
calc_min <- function(x) {
  if (all(is.na(x))) return(NA)
  min(x, na.rm = TRUE)
}
calc_mean <- function(x) {
  if (all(is.na(x))) return(NA)
  mean(x, na.rm = TRUE)
}
calc_max <- function(x) {
  if (all(is.na(x))) return(NA)
  max(x, na.rm = TRUE)
}


## Unit conversions ----

c_to_f <- function(x) x * 1.8 + 32
mm_to_in <- function(x) x / 25.4
cm_to_in <- function(x) x / 2.54
# km_to_mi <- function(x) x / 1.609
kmh_to_mps <- function(x) x / 3.6
mps_to_mph <- function(x) x * 2.237
mbar_to_inHg <- function(x) x / 33.864


## Location helpers ----

#' parse lat/lng coordinates from string
#' @param str input string containing coordinates to parse in form "lat, lng"
#' @returns named list { lat: numeric, lng: numeric }
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

#' returns TRUE if location is within service boundary shapefile
#' @param lat latitude of point
#' @param lng longitude of point
#' @returns boolean
validate_ll <- function(lat, lng) {
  mapply(function(lat, lng) {
    if (!is.numeric(lat) | !is.numeric(lng)) return(F)
    pt <- st_point(c(lng, lat)) %>%
      st_sfc(crs = 4326) %>%
      st_transform(st_crs(service_bounds_3857))
    length(st_intersection(pt, service_bounds_3857)) == 1
  }, lat, lng)
}

#' creates an appropriately sized grid cell based on centroid coordinates
#' @param lat latitude of point
#' @param lng longitude of point
#' @returns sf object
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


# IBM API interface ------------------------------------------------------------

#' Breaks up longer time periods into 1000 hour chunks
#' API will only return 1000 hours at a time
#' @param start_date date or YYYY-MM-DD string
#' @param end_date date or YYYY-MM-DD string
#' @returns list of two-element formatted datetime strings
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
#' API documentation: https://docs.google.com/document/d/13HTLgJDpsb39deFzk_YCQ5GoGoZCO_cRYzIxbwvgJLI/edit?tab=t.0
#' @param lat latitude of point
#' @param lng longitude of point
#' @param start_date date or date string
#' @param end_date date or date string
#' @returns tibble, either with hourly data if successful or empty if failed
get_ibm <- function(lat, lng, start_date, end_date) {
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
      ) %>%
      req_timeout(10)
  })
  resps <- req_perform_parallel(reqs, on_error = "continue", progress = F)
  responses <- lapply(resps, function(resp) {
    tryCatch({
      if (resp_status(resp) != 200) {
        message("Received status ", resp_status(resp), " with message ", resp_status_desc(resp))
        stop()
      }
      resp_body_json(resp, simplifyVector = T) %>% as_tibble()
    }, error = function(e) return(tibble()))
  })
  weather <- bind_rows(responses)
  msg <- str_glue("weather for {lat}, {lng} from {start_date} to {end_date} in {Sys.time() - stime}")
  message(ifelse(nrow(weather) > 0, "OK ==> Got ", "FAIL ==> Could not get "), msg)
  weather
}


#' Does some minimal processing on the IBM response to set local time and date
#' @param ibm_response hourly weather data received from API
#' @returns tibble
clean_ibm <- function(ibm_response) {
  if (nrow(ibm_response) == 0) return(tibble())
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


# Variable selection and unit conversion features ------------------------------

#' all possible, and currently enabled weather columns
#' some are renamed for clarity
ibm_vars <- c(
  "temperature",
  # "temperature_change24hour",
  # "temperature_max24hour",
  # "temperature_min24hour",
  "dew_point" = "temperature_dew_point",
  # "temperature_feels_like",
  "relative_humidity",
  "precip" = "precip1hour",
  # "precip6hour",
  # "precip24hour",
  # "precip2day",
  # "precip3day",
  # "precip7day",
  # "precip_mtd",
  # "precip_ytd",
  "snow" = "snow1hour",
  # "snow6hour",
  # "snow24hour",
  # "snow2day",
  # "snow3day",
  # "snow7day",
  # "snow_mtd",
  # "snow_season",
  # "snow_ytd",
  # "uv_index",
  # "visibility",
  "wind_speed",
  "wind_gust",
  "wind_direction",
  "pressure_mean_sea_level",
  "pressure_change"
)

#' list of weather variables, unit suffixes, and conversion functions
#' all derivative columns of each of these will start with the same text
#' e.g. temperature => temperature_min => temperature_min_30ma
measures <- tribble(
  ~measure, ~metric, ~imperial, ~conversion,
  "temperature", "°C", "°F", c_to_f,
  "dew_point",   "°C", "°F", c_to_f,
  "relative_humidity", "%", "%", \(x) x,
  "precip", "mm", "in", mm_to_in,
  "snow", "cm", "in", cm_to_in,
  "wind_speed", "m/s", "mph", mps_to_mph,
  "wind_gust", "m/s", "mph", mps_to_mph,
  "wind_direction", "°", "°", \(x) x,
  "pressure_mean_sea_level", "mbar", "inHg", mbar_to_inHg,
  "pressure_change", "mbar", "inHg", mbar_to_inHg,
)

#' converts all measures from default metric to imperial values
#' operates on any of the major datasets
convert_measures <- function(df) {
  for (i in 1:nrow(measures)) {
    m <- measures[i,]
    df <- mutate(df, across(starts_with(m$measure), m$conversion[[1]]))
  }
  df
}

#' returns the unit name for the given column
#' used to append unit suffix in plotly
find_unit <- function(col_name, unit_system = c("metric", "imperial")) {
  unit_system <- match.arg(unit_system)
  matched <- measures %>%
    rowwise() %>%
    filter(grepl(measure, col_name))
  if (nrow(matched) == 1) matched[[unit_system]] else ""
}

#' adds the unit suffix to each column name where appropriate
#' for exporting as CSV so unit is documented
rename_with_units <- function(df, unit_system = c("metric", "imperial")) {
  unit_system <- match.arg(unit_system)
  for (i in 1:nrow(measures)) {
    m <- measures[i,]
    df <- df %>%
      rename_with(
        .fn = ~paste(.x, m[[unit_system]], sep = "_", recycle0 = TRUE),
        .cols = starts_with(m$measure)
      )
  }
  clean_names(df)
}


# Field Crops Disease Models ---------------------------------------------------

# Logistic function to convert logit to probability
logistic <- function(logit) exp(logit) / (1 + exp(logit))

#' Apothecial sporecaster, dryland model
#' @param MaxAT_30ma 30-day moving average of daily maximum temperature, Celsius
#' @param MaxWS_30ma 30-day moving average of daily maximum wind speed, m/s
#' @returns probability of spore presence
sporecaster_dry <- function(MaxAT_30ma, MaxWS_30ma) {
  mu <- -0.47 * MaxAT_30ma - 1.01 * MaxWS_30ma + 16.65
  logistic(mu)
}

#' Apothecial sporecaster, irrigated model
#' @param MaxAT_30MA Maximum daily temperature, 30-day moving average, Celsius
#' @param MaxRH_30ma Maximum daily relative humidity, 30-day moving average, 0-100%
#' @param spacing Row spacing, either "15" or "30", inches
#' @returns probability of spore presence
sporecaster_irrig <- function(MaxAT_30MA, MaxRH_30ma, spacing = c("15", "30")) {
  spacing <- match.arg(spacing)
  mu <- -2.38 * (spacing == "30") + 0.65 * MaxAT_30MA + 0.38 * MaxRH_30ma - 52.65
  logistic(mu)
}

#' Frogeye leaf spot model
#' use when growth stage within R1-R5 and no fungicide in last 14 days
#' @param MaxAT_30ma Maximum daily temperature, 30-day moving average, Celsius
#' @param HrsRH80_30ma Daily hours RH > 80%, 30-day moving average, 0-24 hours
#' @returns probability of spore presence
predict_fls <- function(MaxAT_30ma, HrsRH80_30ma) {
  mu <- -5.92485 + 0.12208 * MaxAT_30ma + 0.17326 * HrsRH80_30ma
  logistic(mu)
}

#' Gray leaf spot model
#' use when growth stage V10-R3 and no fungicide in last 14 days
#' @param MinAT_21ma Minimum daily temperature, 21-day moving average, Celsius
#' @param MinDP_30ma Minimum dew point temperature, 30-day moving average, Celsius
#' @returns probability of spore presence
predict_gls <- function(MinAT_21ma, MinDP_30ma) {
  mu <- -2.9467 - 0.03729 * MinAT_21ma + 0.6534 * MinDP_30ma
  logistic(mu)
}

#' Tarspot model
#' @param MeanAT_30ma Mean daily temperature, 30-day moving average, Celsius
#' @param MaxRH_30ma Maximum daily relative humidity, 30-day moving average, 0-100%
#' @param HrsRH90Night_14ma Nighttime hours RH > 90%, 14-day moving average, 0-24 hours
#' @returns probability of spore presence
predict_tarspot <- function(MeanAT_30ma, MaxRH_30ma, HrsRH90Night_14ma) {
  mu1 <- 32.06987 - 0.89471 * MeanAT_30ma - 0.14373 * MaxRH_30ma
  mu2 <- 20.35950 - 0.91093 * MeanAT_30ma - 0.29240 * HrsRH90Night_14ma
  (logistic(mu1) + logistic(mu2)) / 2
}


# Vegetable Disease Models -----------------------------------------------------

#' Potato physiological days
#' More information: https://vegpath.plantpath.wisc.edu/diseases/potato-early-blight/
#' @param tmin Minimum daily temperature, Celsius
#' @param tmax Maximum daily temperature, Celsius
#' @returns numeric daily potato physiological days, approx 0-10 per day
calculate_pdays <- function(tmin, tmax) {
  a = 5 * pday(tmin)
  b = 8 * pday((2 * tmin / 3) + (tmax / 3))
  c = 8 * pday((2 * tmax / 3) + (tmin / 3))
  d = 3 * pday(tmin)
  (a + b + c + d) / 24.0
}

#' P-day function for an individual temperature
#' @param temp temperature in Celsius
#' @returns numeric p-day value
pday <- function(temp) {
  case_when(
    temp < 7 ~ 0,
    between(temp, 7, 21) ~ 10 * (1 - ((temp - 21)^2 / 196)), # 196 = (21-7)^2
    between(temp, 21, 30) ~ 10 * (1 - ((temp - 21)**2 / 81)), # 81 = (30-21)^2
    T ~ 0
  )
}

# expand_grid(tmin = 0:35, tmax = 0:35) %>%
#   mutate(pdays = calculate_pdays(tmin, tmax)) %>%
#   ggplot(aes(x = tmin, y = tmax, fill = pdays)) +
#   geom_tile() +
#   scale_fill_viridis_c() +
#   coord_cartesian(expand = F)


#' Late blight disease severity values
#' based on the Wallins BLITECAST model
#' - https://www.google.com/books/edition/The_Plant_Disease_Reporter/ow9BD6P2KZ4C?hl=en&gbpv=1&dq=wallins%20blitecast&pg=PA95&printsec=frontcover
#' - https://ipm.ucanr.edu/DISEASE/DATABASE/potatolateblight.html
#' More information: https://vegpath.plantpath.wisc.edu/diseases/potato-late-blight/
#' @param temp Mean temperature during hours where RH > 90%, Celsius
#' @param hours Number of hours where RH > 90%
#' @returns numeric 0-4 disease severity values
late_blight_dsv <- function(temp, hours) {
  case_when(
    is.na(temp) | is.na(hours) ~ 0,
    temp < 7.2 ~ 0,
    temp <= 11.6 ~
      (hours > 21) +
      (hours > 18) +
      (hours > 15),
    temp <= 15 ~
      (hours > 21) +
      (hours > 18) +
      (hours > 15) +
      (hours > 12),
    temp <= 26.6 ~
      (hours > 18) +
      (hours > 15) +
      (hours > 12) +
      (hours > 9),
    T ~ 0
  )
}

# expand_grid(temp = 0:30, hours = 0:24) %>%
#   mutate(dsv = late_blight_dsv(temp, hours)) %>%
#   ggplot(aes(x = temp, y = hours, fill = dsv)) +
#   geom_tile() +
#   scale_fill_viridis_c() +
#   coord_cartesian(expand = F)


#' Carrot foliar disease (Alternaria)
#' @param temp Mean temperature during hours where RH > 90%, Celsius
#' @param hours Number of hours where RH > 90%
#' @returns numeric 0-4 dsv
carrot_foliar_dsv <- function(temp, hours) {
  case_when(
    is.na(temp) | is.na(hours) ~ 0,
    temp < 13 ~ 0,
    temp <= 18 ~
      (hours > 20) +
      (hours > 15) +
      (hours > 7),
    temp <= 21 ~
      (hours > 22) +
      (hours > 15) +
      (hours > 8) +
      (hours > 4),
    temp <= 26 ~
      (hours > 20) +
      (hours > 12) +
      (hours > 5) +
      (hours > 2),
    T ~
      (hours > 22) +
      (hours > 15) +
      (hours > 8) +
      (hours > 3)
  )
}

# expand_grid(temp = 0:30, hours = 0:24) %>%
#   mutate(dsv = carrot_foliar_dsv(temp, hours)) %>%
#   ggplot(aes(x = temp, y = hours, fill = dsv)) +
#   geom_tile() +
#   scale_fill_viridis_c() +
#   coord_cartesian(expand = F)


#' Cercospora foliar disease daily infection values
#' based on https://apsjournals.apsnet.org/doi/abs/10.1094/PDIS.1998.82.7.716
#' more information: https://vegpath.plantpath.wisc.edu/diseases/carrot-alternaria-and-cercospora-leaf-blights/
#' @param temp Mean temperature during hours where RH > 90%, Celsius, converted to Fahrenheit internally
#' @param hours Number of hours where RH > 90%
#' @returns 0-7 div
cercospora_div <- function(temp, hours) {
  temp <- c_to_f(temp)
  mapply(
    function(temp, hours) {
      if (is.na(temp) || is.na(hours)) return(0)
      if (temp < 60) return(0)
      if (temp <= 61) {
        return(if (hours <= 21) 0 else 1)
      } else if (temp <= 62) {
        return(if (hours <= 19) 0 else if (hours <= 22) 1 else 2)
      } else if (temp <= 63) {
        return(if (hours <= 16) 0 else if (hours <= 19) 1 else if (hours <= 21) 2 else 3)
      } else if (temp <= 64) {
        return(if (hours <= 13) 0 else if (hours <= 15) 1 else if (hours <= 18) 2
          else if (hours <= 20) 3 else if (hours <= 23) 4 else 5)
      } else if (temp <= 65) {
        return(if (hours <= 6) 0 else if (hours <= 8) 1 else if (hours <= 12) 2
          else if (hours <= 18) 3 else if (hours <= 21) 4 else 5)
      } else if (temp <= 71) {
        return(if (hours <= 3) 0 else if (hours <= 6) 1 else if (hours <= 10) 2
          else if (hours <= 14) 3 else if (hours <= 18) 4 else if (hours <= 21) 5 else 6)
      } else if (temp <= 72) {
        return(if (hours <= 2) 0 else if (hours <= 6) 1 else if (hours <= 9) 2
          else if (hours <= 13) 3 else if (hours <= 17) 4 else if (hours <= 20) 5 else 6)
      } else if (temp <= 73) {
        return(if (hours <= 1) 0 else if (hours <= 6) 1 else if (hours <= 9) 2
          else if (hours <= 12) 3 else if (hours <= 16) 4 else if (hours <= 19) 5 else 6)
      } else if (temp <= 76) {
        return(if (hours <= 5) 1 else if (hours <= 9) 2 else if (hours <= 11) 3
          else if (hours <= 16) 4 else if (hours <= 18) 5 else if (hours <= 23) 6 else 7)
      } else if (temp <= 77) {
        return(if (hours <= 5) 1 else if (hours <= 8) 2 else if (hours <= 12) 3
          else if (hours <= 15) 4 else if (hours <= 18) 5 else if (hours <= 22) 6 else 7)
      } else if (temp <= 78) {
        return(if (hours <= 5) 1 else if (hours <= 8) 2 else if (hours <= 11) 3
          else if (hours <= 14) 4 else if (hours <= 17) 5 else if (hours <= 20) 6 else 7)
      } else if (temp <= 79) {
        return(if (hours <= 4) 1 else if (hours <= 7) 2 else if (hours <= 9) 3
          else if (hours <= 12) 4 else if (hours <= 14) 5 else if (hours <= 17) 6 else 7)
      } else if (temp <= 80) {
        return(if (hours <= 3) 1 else if (hours <= 6) 2 else if (hours <= 8) 3
          else if (hours <= 10) 4 else if (hours <= 12) 5 else if (hours <= 15) 6 else 7)
      } else if (temp <= 81) {
        return(if (hours <= 2) 1 else if (hours <= 4) 2 else if (hours <= 6) 3
          else if (hours <= 7) 4 else if (hours <= 9) 5 else if (hours <= 11) 6 else 7)
      } else if (temp <= 82) {
        return(if (hours <= 2) 1 else if (hours <= 4) 2 else if (hours <= 5) 3
          else if (hours <= 7) 4 else if (hours <= 8) 5 else if (hours <= 10) 6 else 7)
      } else {
        return(if (hours <= 2) 1 else if (hours <= 4) 2 else if (hours <= 5) 3
          else if (hours <= 7) 4 else if (hours <= 8) 5 else if (hours <= 9) 6 else 7)
      }
    },
    temp,
    hours
  )
}

# test
# expand_grid(temp = 15:30, hours = 0:24) %>%
#   mutate(dsv = cercospora_div(temp, hours)) %>%
#   ggplot(aes(x = temp, y = hours, fill = dsv)) +
#   geom_tile() +
#   scale_fill_viridis_c() +
#   coord_cartesian(expand = F)


# Growing degree days ----------------------------------------------------------

#' Single sine method
#' to create GDDs with an upper threshold, calculate GDDs with the upper threshold
#' as the base temperature and subtract that value from the GDDs for the base temp
#' @param tmin minimum daily temperature
#' @param tmax maximum daily temperature
#' @param base base/lower temperature threshold
#' @returns single sine growing degree days for one day
gdd_sine <- function(tmin, tmax, base) {
  mapply(function(tmin, tmax, base) {
    if (is.na(tmin) || is.na(tmax)) return(NA)

    # swap min and max if in wrong order for some reason
    if (tmin > tmax) { t = tmin; tmin = tmax; tmax = t }

    # min and max < lower
    if (tmax <= base) return(0)

    average = (tmin + tmax) / 2

    # tmin > lower = simple average gdds
    if (tmin >= base) return(average - base)

    # tmin < lower, tmax > lower = sine gdds
    alpha = (tmax - tmin) / 2
    base_radians = asin((base - average) / alpha)
    a = average - base
    b = pi / 2 - base_radians
    c = alpha * cos(base_radians)
    (1 / pi) * (a * b + c)
  }, tmin, tmax, base)
}


# Data pipeline ----------------------------------------------------------------

#' Summarize downloaded wether data by grid cell and creates sf object
#' used to intersect site points with existing weather data
#' @param ibm_hourly hourly weather data from `clean_ibm` function
build_grids <- function(ibm_hourly) {
  ibm_hourly %>%
    summarize(
      date_min = min(date),
      date_max = max(date),
      days_expected = as.integer(max(date) - min(date)) + 1,
      days_actual = n_distinct(date),
      days_missing = days_expected - days_actual,
      days_missing_pct = days_missing / days_actual,
      hours_expected = days_expected * 24,
      hours_actual = n(),
      hours_missing = hours_expected - hours_actual,
      hours_missing_pct = hours_missing / hours_actual,
      .by = c(grid_id, grid_lat, grid_lng)
    ) %>%
    rowwise() %>%
    mutate(geometry = ll_to_grid(grid_lat, grid_lng)) %>%
    ungroup() %>%
    st_set_geometry("geometry")
}

#' Creates the working hourly weather dataset from cleaned ibm response
#' @param ibm_hourly hourly weather data from `clean_ibm` function
#' @returns tibble
build_hourly <- function(ibm_hourly) {
  ibm_hourly %>%
    select(
      grid_id, grid_lat, grid_lng,
      datetime_utc, time_zone, datetime_local, date,
      all_of(ibm_vars)
    ) %>%
    mutate(
      yday = yday(date),
      year = year(date),
      month = month(date),
      day = day(date),
      hour = hour(datetime_local),
      night = !between(hour, 7, 19), # night is between 20:00 and 6:00
      date_since_night = as_date(datetime_local + hours(4)),
      .after = date,
    ) %>%
    arrange(grid_lat, grid_lng, datetime_utc) %>%
    mutate(precip_cumulative = cumsum(precip), .after = precip) %>%
    mutate(snow_cumulative = cumsum(snow), .after = snow) %>%
    mutate(dew_point_depression = abs(temperature - dew_point), .after = dew_point) %>%
    # km/hr => m/s
    mutate(across(c(wind_speed, wind_gust), kmh_to_mps))
}

#' Generate daily summary data from hourly weather
#' @param hourly accepts the cleaned hourly data from `build_hourly()`
#' @returns tibble
build_daily <- function(hourly) {
  # grid attributes to be retained
  lat_lng <- hourly %>%
    distinct(grid_id, grid_lat, grid_lng)

  # summarized by calendar date
  summary_fns <- c("min" = calc_min, "mean" = calc_mean, "max" = calc_max)
  by_date <- hourly %>%
    summarize(
      across(c(temperature, dew_point, dew_point_depression, relative_humidity), summary_fns),
      across(c(precip, snow), c("daily" = calc_sum, "max_hourly" = calc_max)),
      across(c(pressure_mean_sea_level, wind_speed), summary_fns),
      wind_gust_max = calc_max(wind_gust),
      across(c(wind_direction), summary_fns),
      hours_rh_over_80 = sum(relative_humidity >= 80),
      hours_rh_over_90 = sum(relative_humidity >= 90),
      hours_missing = 24 - n(),
      .by = c(grid_id, date, yday, year, month, day)
    ) %>%
    mutate(precip_cumulative = cumsum(precip_daily), .after = precip_max_hourly, .by = grid_id ) %>%
    mutate(snow_cumulative = cumsum(snow_daily), .after = snow_max_hourly, .by = grid_id) %>%
    filter(hours_missing < 6) %>%
    select(-hours_missing)

  # summarized by "date since night" eg since 8 pm the day before through 7 pm
  by_night <- hourly %>%
    mutate(
      rh80 = relative_humidity >= 80,
      rh90 = relative_humidity >= 90
    ) %>%
    summarize(
      hours_rh_over_80_night = sum(night & rh80),
      hours_rh_over_90_night = sum(night & rh90),
      temperature_mean_rh_over_80 = if_else(sum(rh80) > 0, sum(temperature * (rh80)) / sum(rh80), NA),
      temperature_mean_rh_over_90 = if_else(sum(rh90) > 0, sum(temperature * (rh90)) / sum(rh90), NA),
      .by = c(grid_id, date_since_night)
    )

  # assemble the data
  by_date %>%
    left_join(by_night, join_by(grid_id, date == date_since_night)) %>%
    left_join(lat_lng, join_by(grid_id)) %>%
    relocate(grid_lat, grid_lng, .after = grid_id) %>%
    arrange(grid_id, date)
}

#' Generate several moving average periods from daily data
#' @param daily accepts daily data from `build_daily()`
#' @returns tibble
build_ma_from_daily <- function(daily, align = c("center", "right")) {
  align <- match.arg(align)

  # retain attribute cols
  attr <- daily %>% select(grid_id, any_of(OPTS$date_attr_cols))

  # define moving average functions
  roll_mean <- function(vec, width) zoo::rollapply(vec, width, \(x) mean(x, na.rm = T), fill = NA, partial = T, align = align)
  fns <- c(
    "7day" = ~roll_mean(.x, 7),
    "14day" = ~roll_mean(.x, 14),
    "21day" = ~roll_mean(.x, 21),
    "30day" = ~roll_mean(.x, 30)
  )

  # apply moving average functions to each primary data column
  ma <- daily %>%
    mutate(
      across(starts_with(c("temperature", "dew_point", "relative_humidity", "wind", "pressure", "hours")), fns),
      .keep = "none"
    )

  # bind attributes
  bind_cols(attr, ma)
}

#' Plant diseases that use moving averages as inputs
#' units must be metric: temperature degC, wind speed m
#' @param ma accepts moving average dataset from `build_ma_from_daily()`
#' @returns tibble
build_disease_from_ma <- function(ma) {
  # retain attribute cols
  attr <- ma %>% select(grid_id, date)

  # calculate disease models from moving averages
  disease <- ma %>%
    mutate(
      sporecaster_dry_probability =
        sporecaster_dry(temperature_max_30day, wind_speed_max_30day),
      sporecaster_irrig_30_probability =
        sporecaster_irrig(temperature_max_30day, relative_humidity_max_30day, "30"),
      sporecaster_irrig_15_probability =
        sporecaster_irrig(temperature_max_30day, relative_humidity_max_30day, "15"),
      frogeye_leaf_spot_probability =
        predict_fls(temperature_max_30day, hours_rh_over_80_30day),
      gray_leaf_spot_probability =
        predict_gls(temperature_min_21day, dew_point_min_30day),
      tarspot_probability =
        predict_tarspot(temperature_mean_30day, relative_humidity_max_30day, hours_rh_over_90_night_14day),
      .keep = "none"
    )

  # bind attributes
  bind_cols(attr, disease)
}

#' Plant diseases that use daily values as inputs
#' @param daily accepts daily dataset from `build_daily()`
#' @returns tibble
build_disease_from_daily <- function(daily) {
  # retain attribute cols
  attr <- daily %>% select(grid_id, date)

  # generate disease models and add cumulative sums where appropriate
  disease <- daily %>%
    mutate(
      potato_pdays = calculate_pdays(temperature_min, temperature_max),
      late_blight_dsv = late_blight_dsv(temperature_mean_rh_over_90, hours_rh_over_90),
      carrot_foliar_dsv = carrot_foliar_dsv(temperature_mean_rh_over_90, hours_rh_over_90),
      cercospora_div = cercospora_div(temperature_mean_rh_over_90, hours_rh_over_90),
      .keep = "none", .by = grid_id
    ) %>%
    mutate(
      across(
        c(potato_pdays, late_blight_dsv, carrot_foliar_dsv, cercospora_div),
        c(cumulative = cumsum)
      ),
      .by = grid_id
    ) %>%
    select(sort(names(.))) %>%
    select(-grid_id)

  # bind attributes
  bind_cols(attr, disease)
}

#' Generate various growing degree day models with and without an 86F upper threshold
#' input temperatures must be Celsius and will be converted to Fahrenheit GDDs
#' @param daily accepts daily dataset from `build_daily()`
#' @returns tibble
build_gdd_from_daily <- function(daily) {
  # retain attribute cols
  attr <- daily %>% select(grid_id, date)

  # convert temperatures
  tmin <- c_to_f(daily$temperature_min)
  tmax <- c_to_f(daily$temperature_max)

  # start with a base 86F model to chopping off the upper thresholds
  gdd <- tibble(base_86 = gdd_sine(tmin, tmax, 86))

  # generate each of the base temperature models with and without the upper threshold
  for (base in c(32, 39.2, 41, 45, 48, 50, 52, 55)) {
    name <- str_replace_all(paste0("base_", base), "\\.", "p")
    gdd[[name]] <- gdd_sine(tmin, tmax, base)
    gdd[[paste0(name, "_upper_86")]] <- gdd[[name]] - gdd$base_86
  }

  # remove the upper threshold model
  gdd$base_86 <- NULL

  # assemble, add cumulative cols, sort names
  bind_cols(attr, gdd) %>%
    mutate(across(starts_with("base_"), c(cumulative = cumsum)), .by = grid_id) %>%
    select(
      all_of(names(attr)),
      all_of(sort(names(.)))
    )
}


# Startup ----------------------------------------------------------------------

## Load files ----

saved_weather <- if (file.exists("data/saved_weather.fst")) {
  as_tibble(read_fst("data/saved_weather.fst"))
}

# EPSG 4326 for use in Leaflet
service_bounds <- read_rds("data/us_ca_clip.rds")

# transform to EPSG 3857 web mercator for intersecting points
service_bounds_3857 <- st_transform(service_bounds, 3857)


## Settings ----

sites_template <- tibble(
  id = integer(),
  name = character(),
  lat = numeric(),
  lng = numeric(),
  temp = logical()
)

OPTS <- lst(
  app_title = "Researcher's Weather Data Tool",
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
  earliest_date = make_date(2015, 1, 1),
  default_start_date = today() - 30,

  # map
  # state_colors = {
  #   pals = RColorBrewer::brewer.pal.info
  #   pal = RColorBrewer::brewer.pal
  #   qual_col_pals = pals[pals$category == 'qual',]
  #   unlist(mapply(pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  # },
  map_bounds_wi = list(
    lat1 = 42.4,
    lat2 = 47.1,
    lng1 = -93.0,
    lng2 = -86.8
  ),
  map_bounds_us = list(
    lat1 = 24.0,
    lat2 = 50.0,
    lng1 = -125.5,
    lng2 = -66.0
  ),
  map_tiles = list(
    "ESRI Topo" = providers$Esri.WorldTopoMap,
    "Satellite" = providers$Esri.WorldImagery,
    "OpenStreetMap" = providers$OpenStreetMap,
    "Grey Canvas" = providers$CartoDB.Positron
  ),
  map_layers = list(
    # counties = "States/Counties"
  ),
  map_click_zoom = 10,

  # allowable names for site loading
  site_cols = c(
    id = "id",
    name = "name",
    name = "location",
    lat = "latitude",
    lng = "longitude",
    lng = "long"
  ),
  max_sites = 10,

  # data types
  data_type_choices = list(
    "Hourly" = "hourly",
    "Daily" = "daily",
    "Moving averages" = "ma",
    "Growing degree days" = "gdd",
    "Disease models" = "disease"
  ),

  # add site_ before some columns in the sites table
  site_attr_rename = {
    cols <- c("id", "name", "lat", "lng")
    names(cols) <- paste0("site_", cols)
    cols
  },

  # plotting
  site_attr_cols = c("site_id", "site_name", "site_lat", "site_lng", "temp"),
  grid_attr_cols = c("grid_id", "grid_lat", "grid_lng", "date_min", "date_max", "days_expected", "days_actual", "days_missing", "days_missing_pct", "hours_expected", "hours_actual", "hours_missing", "hours_missing_pct", "geometry"),
  date_attr_cols = c("datetime_utc", "time_zone", "datetime_local", "date", "yday", "year", "month", "day", "hour", "night", "date_since_night"),
  daily_attr_cols = c("date", "yday", "year", "month", "day"),
  plot_default_cols = c("temperature", "temperature_mean", "temperature_mean_7day", "base_50_upper_86_cumulative"),
  plot_ignore_cols = c(site_attr_cols, grid_attr_cols, date_attr_cols),
  plot_title_font = list(
    family = "Red Hat Display",
    size = 16
  ),
  plot_axis_font = list(
    family = "Red Hat Text",
    size = 14
  )

)

# cat_names(test_hourly)
# cat_names(test_daily)
# cat_names(test_ma)
# cat_names(test_probs)



# Setup ------------------------------------------------------------------------

## County shapefile ----

# prep_counties <- function() {
#   states <- read_sf("prep/cb-2018-conus-state-20m.geojson") %>%
#     clean_names() %>%
#     st_drop_geometry() %>%
#     select(statefp, state_name = name)
#
#   counties_sf <- read_sf("prep/cb-2018-conus-county-5m.geojson") %>%
#     clean_names() %>%
#     select(statefp, countyfp, county_name = name, geometry) %>%
#     left_join(states) %>%
#     relocate(state_name, .after = statefp)
#
#   counties_sf %>% write_rds("data/counties-conus.rds")
# }
#
# prep_counties()


## Data structures ----

# cat_names <- function(df) {
#   message(deparse(substitute(df)))
#   cat("c(")
#   cat(paste(paste0("\"", names(df), "\""), collapse = ", "))
#   cat(")\n")
# }
#
# get_specs <- function() {
#   ibm <- get_ibm(45, -89, today() - 1, today())
#   cat_names(ibm)
#
#   ibm_clean <- clean_ibm(ibm)
#   cat_names(ibm_clean)
#
#   hourly <- build_hourly(ibm_clean)
#   cat_names(hourly)
#
#   daily <- build_daily(hourly)
#   cat_names(daily)
# }
#
# get_specs()
