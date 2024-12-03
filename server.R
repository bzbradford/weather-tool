
server <- function(input, output, session) {

  # Functions ----

  # try to enforce site attributes
  create_site <- function(loc) {
    sites <- rv$sites
    loc$id <- ifelse(nrow(sites) > 0, max(sites$id) + 1, 1)
    loc$temp <- !isTruthy(loc$temp)

    # make sure it has all the attributes
    stopifnot(all(names(sites_template) %in% names(loc)))
    loc <- loc[names(loc) %in% names(sites_template)]
    loc$lat <- round(loc$lat, 4)
    loc$lng <- round(loc$lng, 4)
    req(validate_loc(loc))
    loc
  }

  save_site <- function(site) {
    sites <- rv$sites %>%
      filter(!temp) %>%
      bind_rows(as_tibble(site)) %>%
      distinct(lat, lng, .keep_all = T) %>%
      mutate(id = row_number())
    rv$sites <- sites
    rv$selected_site <- last(sites$id)
  }


  # Reactive values ----

  ## rv ----
  rv <- reactiveValues(
    # IBM hourly weather, lightly modified
    weather = saved_weather,

    # table storing site locations
    sites = sites_template,

    # id of last-clicked site
    selected_site = 1,

    # sidebar site upload UI
    show_upload = FALSE, # toggle upload ui
    upload_msg = NULL, # error message

    # both must be true to show data display
    sites_ready = FALSE,
    weather_ready = FALSE,
  )

  ## selected_dates ----
  selected_dates <- reactive({
    start <- req(input$start_date)
    end <- req(input$end_date)
    req(start <= end)

    list(
      start = start,
      end = end
    )
  })

  ## sites_df ----
  sites_df <- reactive({
    if (input$multi_site)
      rv$sites
    else
      rv$sites %>% filter(id == rv$selected_site)
  })

  ## sites_sf ----
  sites_sf <- reactive({
    sites <- sites_df()
    req(nrow(sites) > 0)
    sites %>%
      st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = F)
  })

  ## wx_hourly ----
  wx_hourly <- reactive({
    weather <- req(rv$weather)
    req(nrow(weather) > 0)
    build_hourly(weather)
  })

  ## wx_grids ----
  wx_grids <- reactive({
    wx_hourly() %>%
      build_grids()
  })

  ## sites_with_grid ----
  sites_with_grid <- reactive({
    sites_sf() %>%
      st_join(wx_grids()) %>%
      st_set_geometry(NULL)
  })

  ## wx_daily ----
  wx_daily <- reactive({
    wx_hourly() %>%
      build_daily()
  })



  # Sidebar UI ----

  ## site_ui // renderUI ----
  output$site_ui <- renderUI({
    id <- ifelse(input$multi_site, "multi_site_ui", "single_site_ui")
    div(
      style = "margin-bottom: 1em;",
      uiOutput(id)
    )
  })

  ## single_site_ui // renderUI ----
  output$single_site_ui <- renderUI({
    div(
      class = "site-tbl-container",
      tableOutput("sites_tbl")
    )
  })

  ## multi_site_ui // renderUI ----
  output$multi_site_ui <- renderUI({
    div(
      p(em("Load or queue up multiple sites. A clicked or searched location must be clicked again to save it to the list.")),

      uiOutput("temp_site_ui"),
      tags$label("Saved sites:"),
      div(
        class = "site-tbl-container",
        tableOutput("sites_tbl")
      ),
      p(
        actionButton("load_example", "Test sites", class = "btn-small"),
        actionButton("upload_csv", "Upload csv"),
        actionButton("clear_sites", "Clear sites")
      ),
      uiOutput("file_upload_ui"),
    )
  })

  ## temp_site_ui // renderUI ----
  output$temp_site_ui <- renderUI({
    site <- sites_df() %>% filter(temp)
    req(nrow(site) > 0)

    div(
      class = "site-tbl-container",
      tags$label("Temporary site:"),
      tableOutput("temp_site_tbl")
    )
  })

  ## temp_site_tbl // renderTable ----
  output$temp_site_tbl <- renderTable({
    sites_df() %>%
      filter(temp) %>%
      select(id, name, lat, lng) %>%
      clean_names("title")
  })

  ## sites_tbl // renderTable ----
  output$sites_tbl <- renderTable({
    sites <- sites_df()
    if (input$multi_site) sites <- sites %>% filter(!temp)
    validate(need(nrow(sites) > 0, "No saved sites. Load a list of sites, or click on a temporary site to save it."))
    sites %>%
      select(id, name, lat, lng) %>%
      clean_names("title")
  })

  ## file_upload_ui // renderUI ----
  output$file_upload_ui <- renderUI({
    req(rv$show_upload)
    div(
      tags$label("Upload csv"), br(),
      em("Upload a csv with three columns: name/location, lat/latitude, lng/long/longitude. Latitude and longitude must be in +/- decimal degrees."),
      fileInput(
        inputId = "sites_csv",
        label = NULL,
        accept = ".csv"
      ),
      { if (!is.null(rv$upload_msg)) div(class = "shiny-error", rv$upload_msg) }
    )
  })

  ## Handle sites upload ----

  observe({
    rv$show_upload <- !rv$show_upload
  }) %>% bindEvent(input$upload_csv)

  # try read sites from csv. max 100 sites
  load_sites <- function(df) {
    df <- df %>%
      clean_names() %>%
      select(any_of(OPTS$site_cols)) %>%
      drop_na()
    req(c("name", "lat", "lng") %in% names(df))
    df <- df %>%
      filter(validate_ll(lat, lng)) %>%
      mutate(id = row_number(), .before = 1) %>%
      mutate(temp = FALSE) %>%
      head(100)
    req(nrow(df) > 0)
    rv$selected_site <- 1
    df
  }

  observe({
    upload <- req(input$sites_csv)
    tryCatch({
      rv$sites <- upload$datapath %>%
        read_csv(show_col_types = F) %>%
        load_sites()
      fit_sites()
      rv$show_upload <- FALSE
      rv$upload_msg <- NULL
    }, error = function(e) {
      rv$upload_msg = "Failed to load sites from csv, please try again."
    })
  })

  ## Handle test site load ----
  observe({
    rv$sites <- read_csv("data/example-sites.csv", show_col_types = F) %>%
      load_sites()
    fit_sites()
  }) %>% bindEvent(input$load_example)

  ## Handle clear sites button ----
  observe({
    rv$sites <- sites_template
    rv$selected_site <- 1
  }) %>% bindEvent(input$clear_sites)


  # Sidebar date selector ----

  ## date_ui // renderUI ----
  output$date_ui <- renderUI({
    today <- Sys.Date()
    dates <- as_date(c(
      coalesce(input$start_date, OPTS$default_start_date),
      coalesce(input$end_date, today)
    ))
    div(
      dateInput(
        inputId = "start_date",
        label = "Start date:",
        min = OPTS$earliest_date,
        max = today,
        value = min(dates)
      ),
      dateInput(
        inputId = "end_date",
        label = "End date:",
        min = OPTS$earliest_date,
        max = today,
        value = max(dates)
      )
    )
  })

  # Sidebar - Fetch weather ----

  ## action_ui // renderUI ----
  output$action_ui <- renderUI({
    btn <- function(msg, ...) actionButton("get", msg, width = "100%", ...)
    sites <- sites_df()
    if (nrow(sites) == 0) {
      btn("No sites selected", disabled = TRUE)
    } else {
      btn("Fetch weather")
    }
  })

  ## Handle fetching ----
  observe({
    sites <- sites_sf()
    wx <- as_tibble(rv$weather)
    dates_need <- seq.Date(selected_dates()$start, selected_dates()$end, 1)
    disable("get")

    # for each site download necessary weather data
    for (i in 1:nrow(sites)) {
      site <- sites[i,]

      # already have some weather?
      if (nrow(wx) > 0) {
        grids <- build_grids(wx)
        grid_dates <- wx %>%
          distinct(grid_id, date)
        dates_have <- site %>%
          st_join(grids) %>%
          left_join(grid_dates, join_by(grid_id)) %>%
          pull(date)
        dates <- as_date(setdiff(dates_need, dates_have))
      } else {
        dates <- dates_need
      }

      # get weather if needed
      if (length(dates) > 0) {
        resp <- get_ibm(site$lat, site$lng, first(dates), last(dates))
        new_wx <- clean_ibm(resp)
        wx <- bind_rows(wx, new_wx) %>%
          distinct(grid_id, datetime_utc, .keep_all = T)
      }
    }

    rv$weather <- wx
    write_fst(wx, "saved_weather.fst", compress = 90)
    enable("get")
  }) %>%
    bindEvent(input$get)



  # Map UI ----

  #' @param map leaflet map to add basemaps
  add_basemaps <- function(map) {
    basemaps <- OPTS$map_tiles
    for (name in names(basemaps)) {
      map <- addProviderTiles(map, basemaps[[name]], group = name)
    }
    map
  }

  #* @param map leaflet proxy object
  #* @param bounds named list { lat1, lat2, lng1, lng2 }
  #* @param options leaflet zoom/pan options
  fit_bounds <- function(map = leafletProxy("map"), bounds, options = NULL) {
    args <- as.list(bounds)
    args$map <- map
    args$options = options
    do.call(fitBounds, args)
  }

  ## map // renderLeaflet ----
  output$map <- renderLeaflet({
    btn1 <- easyButton(
      position = "topleft",
      icon = "fa-location",
      title = "Show my location on the map",
      onClick = JS("(btn, map) => { Shiny.setInputValue('map_btn', 'user_loc', {priority: 'event'}); }")
    )

    btn2 <- easyButton(
      position = "topleft",
      icon = "fa-expand",
      title = "Zoom to sites",
      onClick = JS("(btn, map) => { Shiny.setInputValue('map_btn', 'zoom_sites', {priority: 'event'}); }")
    )

    btn3 <- easyButton(
      position = "topleft",
      icon = "fa-globe",
      title = "Reset map view",
      onClick = JS("(btn, map) => { Shiny.setInputValue('map_btn', 'zoom_extent', {priority: 'event'}); }")
    )

    leaflet(options = leafletOptions(preferCanvas = T)) %>%
      add_basemaps() %>%
      fit_bounds(OPTS$map_bounds_wi) %>%
      addMapPane("counties", 410) %>%
      addMapPane("grid", 420) %>%
      addMapPane("sites", 430) %>%
      addLayersControl(
        baseGroups = names(OPTS$map_tiles),
        # overlayGroups = unlist(OPTS$map_layers, use.names = F),
        options = layersControlOptions(collapsed = T)
      ) %>%
      addEasyButtonBar(btn1, btn2, btn3) %>%
      # assign leaflet map object to global var 'map'
      htmlwidgets::onRender("() => { map = this; }") %>%
      suspendScroll(
        sleepTime = 0,
        wakeTime = 1000,
        hoverToWake = T,
        sleepNote = F,
        sleepOpacity = 1
      )
  })

  ## searchbox_ui // renderUI ----
  output$searchbox_ui <- renderUI({
    div(
      HTML(paste0("<script async src='https://maps.googleapis.com/maps/api/js?key=", OPTS$google_key, "&loading=async&libraries=places&callback=initAutocomplete'></script>")),
      textInput("searchbox", "Find a location by name")
    )
  })

  ## coord_search_ui // renderUI ----
  # Coordinate searchbox under map
  output$coord_search_ui <- renderUI({
    runjs("
      $(document).keyup((event) => {
        if ($('#coord_search').is(':focus') && (event.key == 'Enter')) {
          $('#coord_search_go').click();
        }
      });
    ")
    div(
      div(tags$label("Find a location by coordinates")),
      div(
        style = "display: inline-flex; gap: 5px; max-width: 100%;",
        textInput(
          inputId = "coord_search",
          label = NULL,
          placeholder = "Enter coordinates"
        ),
        div(
          style = "margin-bottom: 10px;",
          actionButton("coord_search_go", "Go")
        )
      )
    )
  })


  # Map layers ----

  ## Show site markers ----
  observe({
    map <- leafletProxy("map")
    clearGroup(map, "sites")
    clearGroup(map, "temp")
    sites <- sites_with_grid()
    req(nrow(sites) > 0)
    sites <- sites %>%
      mutate(
        group = if_else(temp, "temp", "sites"),
        icon = case_when(
          input$multi_site & temp ~ "plus",
          selected_dates()$start < date_min |
            selected_dates()$end > date_max |
            is.na(days_missing) |
            days_missing > 0 ~ "download",
          input$multi_site & !temp ~ as.character(id),
          T ~ "check"),
        marker_color = if_else(id == rv$selected_site, "red", "blue"),
        label = paste0(
          "<b>Site ", id, ": ", name, "</b><br>",
          sprintf("%.4f, %.4f", lat, lng), "<br>",
          if_else(icon == "download", "Download required", "Data ready"),
          if_else(input$multi_site & id == rv$selected_site, "<br>Selected", ""),
          if_else(input$multi_site & temp, "<br>Temporary site", "")
        ) %>% lapply(HTML)
      )
    req(nrow(sites) > 0)
    leafletProxy("map") %>%
      addAwesomeMarkers(
        data = sites,
        lat = ~lat,
        lng = ~lng,
        label = ~label,
        layerId = ~id,
        group = ~group,
        icon = ~makeAwesomeIcon(
          library = "fa",
          icon = icon,
          markerColor = marker_color,
          iconColor = "#fff"),
        options = markerOptions(pane = "sites")
      )
  })

  ## Show weather data grids ----
  observe({
    grids <- wx_grids() %>%
      mutate(label = paste0(
        "<b>Downloaded weather grid</b><br>",
        "Earliest date: ", date_min, "<br>",
        "Latest date: ", date_max, "<br>",
        "Total days: ", days_expected, "<br>",
        "Missing days: ", days_missing, sprintf(" (%.1f%%)", days_missing_pct), "<br>",
        "Missing hours: ", hours_missing, sprintf(" (%.1f%%)", hours_missing_pct), "<br>",
        "Center latitude: ", sprintf("%.2f", grid_lat), "<br>",
        "Center longitude: ", sprintf("%.2f", grid_lng)
      ) %>% lapply(HTML))
    leafletProxy("map") %>%
      addPolygons(
        data = grids,
        weight = 1,
        label = ~label,
        layerId = ~grid_id,
        group = "grid",
        options = pathOptions(pane = "grid")
      )
  })


  # Map handlers ----

  fly_to <- function(loc) {
    leafletProxy("map") %>%
      flyTo(loc$lng, loc$lat, max(10, isolate(input$map_zoom)))
  }

  fit_sites <- function() {
    sites <- sites_df()
    req(nrow(sites) > 0)
    bounds <- list(
      lat1 = min(sites$lat),
      lat2 = max(sites$lat),
      lng1 = min(sites$lng),
      lng2 = max(sites$lng)
    )
    fit_bounds(bounds = bounds, options = list(padding = c(100, 100), maxZoom = 10))
  }

  ## Handle EasyButton clicks ----
  observe({
    btn <- req(input$map_btn)
    map <- leafletProxy("map")

    if (btn == "user_loc") {
      runjs("
        map.getMap().locate({ setView: false }).on('locationfound', (event) => {
          Shiny.setInputValue('user_loc', event.latlng, {priority: 'event'})
        })")
    } else if (btn == "zoom_sites") {
      fit_sites()
    } else if (btn == "zoom_extent") {
      fit_bounds(bounds = OPTS$map_bounds_us)
    }
  })

  ## Handle coord search button ----
  # try to parse coords and save if it works
  observe({
    str <- req(input$coord_search)
    try({
      coords <- parse_coords(str)
      coord_hash <- paste0("{name: 'Searched', lat:", coords$lat, ", lng:", coords$lng, "}")
      cmd <- paste0("Shiny.setInputValue('searched_loc', ", coord_hash, ", {priority: 'event'})")
      runjs(cmd)
    })
  }) %>%
    bindEvent(input$coord_search_go)

  ## Handle searched location from google or coordinates ----
  # name is already set by script
  observe({
    loc <- req(input$searched_loc)
    site <- create_site(loc)
    save_site(site)
    fly_to(loc)
    runjs("
      document.getElementById('searchbox').value = '';
      document.getElementById('coord_search').value = '';
    ")
  }) %>%
    bindEvent(input$searched_loc)

  ## Handle geolocation ----
  observe({
    loc <- req(input$user_loc)
    loc$name <- "Geolocated"
    site <- create_site(loc)
    save_site(site)
    fly_to(site)
  }) %>%
    bindEvent(input$user_loc)

  ## Handle location from click ----
  observe({
    loc <- req(input$map_click)
    loc$name <- "Clicked point"
    site <- create_site(loc)
    save_site(site)
    fly_to(site)
  }) %>%
    bindEvent(input$map_click$.nonce)

  ## Handle marker click ----
  observe({
    marker <- req(input$map_marker_click)
    rv$selected_site <- marker$id
    if (input$multi_site && marker$group == "temp") {
      rv$sites <- rv$sites %>% mutate(temp = FALSE)
    }
    fly_to(marker)
  }) %>%
    bindEvent(input$map_marker_click$.nonce)



  # Data display ----

  ## selected_data // reactive ----
  selected_data <- reactive({
    sites <- sites_with_grid()
    type <- req(input$data_type)
    data <- if (type == "hourly") wx_hourly()
      else if (type == "daily") wx_daily()
      else rv$weather %>% arrange(datetime_utc)
    data <- data %>%
      filter(date > selected_dates()$start) %>%
      filter(date < selected_dates()$end)
    sites %>%
      drop_na(grid_id) %>%
      select(-c(grid_lat, grid_lng)) %>%
      left_join(data, join_by(grid_id)) %>%
      select(-grid_id) %>%
      rename(all_of(site_attr)) %>%
      mutate(across(where(is.numeric), ~signif(.x)))
  })

  observe({
    sr <- nrow(sites_df()) > 0
    wr <- nrow(selected_data()) > 0
    if (rv$sites_ready != sr) rv$sites_ready <- sr
    if (rv$weather_ready != wr) rv$weather_ready <- wr
  })

  site_attr <- {
    cols <- c("id", "name", "lat", "lng")
    names(cols) <- paste0("site_", cols)
    cols
  }

  format_dt <- function(df) {
    df %>%
      rename(all_of(site_attr)) %>%
      mutate(across(where(is.numeric), ~signif(.x))) %>%
      mutate(across(any_of(c("datetime_utc", "datetime_local")), as.character)) %>%
      select(-grid_id) %>%
      clean_names("big_camel")
  }

  ## data_ui // renderUI ----
  output$data_ui <- renderUI({
    tagList(
      p(em("Selected weather parameters and additional generated data columns. All units are metric. Temperature and dew point: degrees Celsius, relative humidity: %, wind speed: meters/second, precipitation: mm, snow: cm, visibility: km.")),
      radioGroupButtons(
        "data_type",
        label = "Dataset",
        choices = OPTS$data_type_choices
      ),
      uiOutput("dataset_ui")
    )
  })

  ## dataset_ui // renderUI ----
  output$dataset_ui <- renderUI({
    validate(need(rv$sites_ready, "No sites selected, click on the map or load sites in the sidebar."))
    validate(need(rv$weather_ready, "No weather data downloaded yet for the selected sites."))

    tagList(
      uiOutput("data_msg"),
      h4("Data chart"),
      uiOutput("plot_ui"),
      bsCollapse(
        bsCollapsePanel(
          title = "View data table",
          DTOutput("data_dt")
        )
      ),
      downloadButton("download_data", "Download dataset")
    )
  })

  ## data_msg // renderUI ----
  output$data_msg <- renderUI({
    sites <- sites_with_grid()
    tests <- c(
      missing_grid = any(is.na(sites$grid_id)),
      missing_internal = any(sites$days_missing > 0),
      missing_early = any(sites$date_min > selected_dates()$start),
      missing_late = any(sites$date_max < selected_dates()$end)
    )
    req(any(tests))
    span(style = "color: red;", "Some sites are missing data based on your date selections, click 'Fetch weather' to load missing data.")
  })

  ## plot_ui // renderUI ----
  output$plot_ui <- renderUI({
    div(
      uiOutput("plot_sites"),
      uiOutput("plot_cols"),
      plotlyOutput("data_plot")
    )
  })

  ## plot_sites // renderUI ----
  output$plot_sites <- renderUI({
    sites <- sites_df()
    req(input$multi_site)
    choices <- set_names(sites$id, sprintf("Site %s", sites$id))
    checkboxGroupInput(
      inputId = "plot_sites",
      label = "Sites to display",
      choices = choices,
      selected = rv$selected_site,
      inline = TRUE
    )
  })

  ## plot_cols // renderUI ----
  output$plot_cols <- renderUI({
    plot_cols <- OPTS$plot_cols[[req(input$data_type)]]
    names(plot_cols) <- make_clean_names(plot_cols, "title")
    checkboxGroupInput(
      inputId = "plot_cols",
      label = "Weather parameters",
      choices = plot_cols,
      selected = plot_cols[1],
      inline = TRUE
    )
  })

  ## data_plot // renderPlotly ----
  output$data_plot <- renderPlotly({
    df <- selected_data()
    opts <- list(
      cols = req(input$plot_cols),
      mode = ifelse(nrow(df) <= 250, "lines+markers", "lines"),
      site_ids = unique(df$site_id),
      selected_ids = input$plot_sites
    )

    req(nrow(df) > 0)
    req(all(opts$cols %in% names(df)))
    if (input$multi_site) req(opts$selected_ids)

    if ("datetime_local" %in% names(df)) df$date <- df$datetime_local

    col_ranges <- df %>%
      summarize(across(all_of(opts$cols), ~max(.x, na.rm = T))) %>%
      pivot_longer(everything()) %>%
      mutate(across(value, log1p)) %>%
      mutate(y2 = value >= mean(value) - .5) %>%
      mutate(y2 = if (mean(y2) > .5) !y2 else y2) %>%
      mutate(axis = if_else(y2, "y2", "y1"))

    y1_title <- filter(col_ranges, axis == "y1")$name %>%
      make_clean_names("title") %>%
      paste(collapse = ", ")
    y2_title <- filter(col_ranges, axis == "y2")$name %>%
      make_clean_names("title") %>%
      paste(collapse = ", ")

    plt <- plot_ly() %>%
      layout(
        hovermode = "x unified",
        showlegend = TRUE,
        margin = list(t = 50, r = 50),
        legend = list(orientation = "h"),
        yaxis = list(
          title = y1_title
        ),
        yaxis2 = list(
          title = y2_title,
          overlaying = "y",
          side = "right"
        )
      )

    for (col in opts$cols) {
      col_name <- make_clean_names(col, "title")
      col_axis <- filter(col_ranges, name == col)$axis

      if (input$multi_site) {
        for (id in opts$selected_ids) {
          site_df <- df %>% filter(site_id == id)
          plt <- add_trace(
            plt,
            x = site_df$date,
            y = site_df[[col]],
            name = sprintf("Site %s: %s", id, col_name),
            type = "scatter",
            mode = opts$mode,
            yaxis = col_axis,
            line = list(shape = "spline")
          )
        }
      } else {
        plt <- add_trace(
          plt,
          x = df$date,
          y = df[[col]],
          name = col_name,
          type = "scatter",
          mode = opts$mode,
          yaxis = col_axis,
          line = list(shape = "spline")
        )
      }
    }

    plt
  })

  ## data_dt // renderDT ----
  output$data_dt <- renderDT({
    selected_data() %>%
      mutate(across(any_of(c("datetime_utc", "datetime_local")), as.character))
  },
    extensions = "FixedColumns",
    options = list(
      autoWidth = TRUE,
      dom = "trip",
      scrollResize = TRUE,
      scrollX = TRUE,
      scrollY = 400,
      scrollCollapse = TRUE,
      pageLength = 24,
      fixedColumns = list(leftColumns = 1)
    )
  )

  ## download_data // downloadHandler ----
  output$download_data <- downloadHandler(
    filename = function() paste(req(input$data_type), "weather.csv"),
    content = function(file) {
      selected_data() %>%
        mutate(across(any_of(c("datetime_utc", "datetime_local")), as.character)) %>%
        clean_names("big_camel") %>%
        write_excel_csv(file, na = "")
    }
  )

}


