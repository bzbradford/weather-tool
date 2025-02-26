
server <- function(input, output, session) {

  # Functions ----

  # try to enforce site attributes
  create_site <- function(loc) {
    sites <- rv$sites
    loc$id <- create_id(sites$id)
    loc$temp <- !isTruthy(loc$temp)
    # if (!input$multi_site) loc$temp <- FALSE

    # make sure it has all the attributes
    stopifnot(all(names(sites_template) %in% names(loc)))
    loc <- loc[names(loc) %in% names(sites_template)]
    loc$lat <- round(loc$lat, 4)
    loc$lng <- round(loc$lng, 4)
    req(validate_ll(loc$lat, loc$lng))
    loc
  }

  save_site <- function(site) {
    if (nrow(rv$sites) == OPTS$max_sites) return()
    sites <- rv$sites %>%
      filter(!temp) %>%
      bind_rows(as_tibble(site)) %>%
      distinct(lat, lng, .keep_all = T) %>%
      mutate(id = row_number())
    rv$sites <- sites
    rv$selected_site <- last(sites$id)
  }

  # observe(echo(rv$sites))
  # observe(echo(rv$selected_site))


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

    start_date = NULL,
    end_date = NULL,

    # for controlling messages on modules
    sites_ready = FALSE,
    weather_ready = FALSE,
  )

  ## control if the data view is ready ----
  observe({
    sr <- nrow(rv$sites) > 0
    if (rv$sites_ready != sr) rv$sites_ready <- sr
  })

  observe({
    wr <- nrow(wx_data()$hourly) > 0
    if (rv$weather_ready != wr) rv$weather_ready <- wr
  })

  ## selected_dates ----
  observe({
    start <- req(input$start_date)
    end <- req(input$end_date)
    req(start <= end)
    rv$status_msg <- NULL
    rv$start_date <- start
    rv$end_date <- end
  })

  selected_dates <- reactive({
    list(
      start = req(rv$start_date),
      end = req(rv$end_date)
    )
  })

  selected_weather <- reactive({
    wx <- rv$weather
    req(nrow(wx) > 0)
    dates <- selected_dates()
    wx %>% filter(between(date, dates$start, dates$end))
  })

  ## wx_grids ----
  wx_grids <- reactive({
    wx <- rv$weather
    req(nrow(wx) > 0)
    build_grids(wx)
  })

  ## sites_sf ----
  sites_sf <- reactive({
    sites <- rv$sites
    req(nrow(sites) > 0)
    wx <- rv$weather
    sf <- sites %>%
      st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = F)

    if (nrow(wx) > 0) {
      sf %>% st_join(wx_grids())
    } else {
      sf %>% mutate(grid_id = NA)
    }
  })

  ## wx_status ----
  # will be blocked when no weather or weather in date range
  wx_status <- reactive({
    wx <- rv$weather
    req(nrow(wx) > 0)
    dates <- selected_dates()
    weather_status(wx, dates$start, dates$end)
  })

  ## grids_with_status ----
  # will be blocked when no weather in date range
  grids_with_status <- reactive({
    status <- wx_status()
    wx_grids() %>%
      filter(grid_id %in% status$grid_id) %>%
      left_join(status, join_by(grid_id))
  })

  ## sites_with_status ----
  # will be blocked when no weather in date range
  sites_with_status <- reactive({
    sites_sf() %>%
      left_join(wx_status(), join_by(grid_id)) %>%
      replace_na(list(needs_download = TRUE))
  })

  ## wx_data ----
  # will be blocked when no weather
  # will return only sites/dates/hourly when no weather in date range

  # these inputs uniquely determine the weather data output
  wx_hash <- reactive({
    list(
      weather = rlang::hash(rv$weather),
      sites = rlang::hash(rv$sites),
      start_date = input$start_date,
      end_date = input$end_date
    )
  })

  wx_data <- reactive({
    weather <- rv$weather
    sites <- sites_with_status()
    dates <- selected_dates()
    req(nrow(weather) > 0)
    req(nrow(sites) > 0)

    wx <- list()
    wx$sites <- sites
    wx$dates <- dates
    wx$hourly <- weather %>%
      filter(grid_id %in% sites$grid_id) %>%
      filter(between(date, dates$start, dates$end)) %>%
      build_hourly()

    if (nrow(wx$hourly) == 0) return(wx)

    wx$daily <- build_daily(wx$hourly)
    wx$ma_center <- build_ma_from_daily(wx$daily, "center")
    wx$ma_right <- build_ma_from_daily(wx$daily, "right")
    wx$disease <- left_join(
      build_disease_from_daily(wx$daily),
      build_disease_from_ma(wx$ma_right),
      join_by(grid_id, date)
    )
    wx$gdd <- build_gdd_from_daily(wx$daily)

    wx
  }) %>% bindCache(wx_hash())


  ## Cookie handling ----

  set_cookie <- function(sites) {
    sites_json <- jsonlite::toJSON(sites)
    runjs(str_glue('setCookie({sites_json})'))
  }

  delete_cookie <- function() {
    runjs("deleteCookie()")
  }

  # on load read cookie data then start cookie writer
  observe({
    runjs("sendCookieToShiny()")

    cookie_writer <- observe({
      sites <- rv$sites
      if (nrow(sites) > 0) {
        set_cookie(sites)
      } else {
        delete_cookie()
      }
    })
  })

  ### Parse sites from cookie ----
  observeEvent(input$cookie, {
    cookie <- req(input$cookie)
    tryCatch({
      cookie_sites <- jsonlite::fromJSON(cookie)
      sites <- cookie_sites %>%
        select(all_of(names(sites_template))) %>%
        filter(validate_ll(lat, lng)) %>%
        distinct() %>%
        head(OPTS$max_sites)
      req(nrow(sites) > 0)
      rv$sites <- sites
      rv$selected_site <- first(sites$id)
      fit_sites()
      showNotification(paste("Loaded", nrow(sites), ifelse(nrow(sites) == 1, "site", "sites"), "from a previous session."))
    }, error = function(e) {
      message("Failed to read sites from cookie: ", e)
      delete_cookie()
    })
  })


  # Help UI ----

  observe({
    mod <- modalDialog(
      title = OPTS$app_title,
      includeMarkdown("about.md"),
      footer = modalButton("Close"),
      easyClose = TRUE
    )
    showModal(mod)
  }) %>% bindEvent(input$help)


  # Sidebar UI ----

  ## Sites table ----

  output$site_help_ui <- renderUI({
    sites <- rv$sites
    text <- if (nrow(sites) == 0) {
      "You don't have any sites. Click on the map or use the search boxes at the bottom of the map to set a location."
    } else if (nrow(sites) == 1) {
      "Use the pin icon to hold a site and add additional sites. Then click on the map or use the search boxes at the bottom of the map to set a location."
    } else if (any(sites$temp)) {
      "Use the pin icon to hold a site and add additional sites. Change the name of a pinned site using the pen icon. Clicking on the map or using the search boxes will replace the last site on the list unless you pin it."
    } else {
      "Change the name of your pinned sites using the pen icon. Click on the map or use the search boxes to add another location."
    }
    div(
      style = "font-style:italic;",
      text
    )
  })

  ### sites_tbl_data // reactive----
  # sites formatted for DT
  sites_tbl_data <- reactive({
    rv$sites %>%
      mutate(
        id = as.character(id),
        across(c(lat, lng), ~sprintf("%.2f", .x)),
        # loc = sprintf("%.2f, %.2f", lat, lng),
        btns = paste0(
          "<div style='display:inline-flex; gap:10px; padding: 5px;'>",
          if_else(temp, site_action_link("save", id), site_action_link("edit", id, name)),
          site_action_link("trash", id),
          "</div>"
        ) %>% lapply(HTML),
        name = sanitize_loc_names(name)
      ) %>%
      select(id, name, lat, lng, btns)
  })

  ### sites_tbl // renderDT ----

  # render initial DT
  output$sites_tbl <- renderDT({
    sites <- isolate(sites_tbl_data())
    selected <- isolate(rv$selected_site)
    datatable(
      sites,
      colnames = c("ID", "Name", "Lat", "Lng", ""),
      rownames = FALSE,
      selection = "none",
      class = "compact",
      options = list(
        dom = "t",
        ordering = FALSE,
        paging = FALSE,
        scrollX = TRUE,
        scrollCollapse = TRUE,
        columnDefs = list(
          list(width = "5%", targets = c(0)),
          list(width = "40%", targets = 1),
          list(width = "15%", targets = c(2, 3)),
          list(className = "dt-right", targets = 2)
        )
      )
    )
  })

  ### refresh sites_tbl // observer ----
  observe({
    df <- sites_tbl_data()
    dataTableProxy("sites_tbl") %>%
      replaceData(df, rownames = FALSE, clearSelection = "none")
  })

  # observe({
  #   selected <- req(rv$selected_site)
  #   runjs("$('#sites_tbl table.dataTable tr').removeClass('selected')")
  #   runjs(sprintf("$('#sites_tbl table.dataTable tr:nth-child(%s)').addClass('selected')", selected))
  # })

  ### Handle save_site button ----
  # only ever 1 temporary site so just make all sites saved
  observeEvent(input$save_site, {
    rv$sites <- rv$sites %>% mutate(temp = FALSE)
  })

  ### Handle trash_site button ----
  observeEvent(input$trash_site, {
    to_delete_id <- req(input$trash_site)
    rv$sites <- rv$sites %>% filter(id != to_delete_id)
  })

  ### Handle edit_site button ----
  observeEvent(input$edit_site, {
    edits <- req(input$edit_site)
    sites <- rv$sites
    sites$name[edits$id] <- edits$name
    rv$sites <- sites
  })


  ## Site list buttons ----

  ### site_btns // renderUI ----
  output$site_btns <- renderUI({
    btn <- function(id, label, ...) actionButton(id, label, class = "btn-sm", ...)
    sites <- isolate(rv$sites)
    div(
      style = "margin-top: 10px;",
      div(
        class = "flex-across",
        # btn("load_example", "Test sites"),
        btn("upload_csv", "Upload csv"),
        btn("clear_sites", "Clear sites", disabled = nrow(sites) == 0),
        if (nrow(sites) == 0) {
          downloadButton("export_sites", "Export sites", class = "btn-sm", disabled = TRUE)
        } else {
          downloadButton("export_sites", "Export sites", class = "btn-sm")
        }
      ),
      uiOutput("file_upload_ui")
    )
  })

  # disable buttons when no sites
  observe({
    sites <- rv$sites
    if (nrow(sites) == 0) {
      disable("clear_sites")
      disable("export_sites")
    } else {
      enable("clear_sites")
      enable("export_sites")
    }
  })

  ### file_upload_ui // renderUI ----
  output$file_upload_ui <- renderUI({
    req(rv$show_upload)
    div(
      style = "margin-top: 10px;",
      tags$label("Upload csv"), br(),
      div(
        style = "font-style: italic;",
        "Upload a csv with columns: name, lat/latitude, lng/long/longitude. Latitude and longitude must be in +/- decimal degrees. Maximum of 10 sites."
      ),
      div(
        style = "margin-top: 10px;",
        fileInput(
          inputId = "sites_csv",
          label = NULL,
          accept = ".csv"
        )
      ),
      { if (!is.null(rv$upload_msg)) div(class = "shiny-error", rv$upload_msg) }
    )
  })

  ### Handle file upload ----
  observe({
    rv$show_upload <- !rv$show_upload
  }) %>% bindEvent(input$upload_csv)

  observe({
    upload <- req(input$sites_csv)
    tryCatch({
      new_sites <- load_sites(upload$datapath)
      rv$sites <- new_sites
      rv$selected_site <- first(new_sites$id)
      fit_sites()
      rv$show_upload <- FALSE
      rv$upload_msg <- NULL
    }, error = function(e) {
      message("File upload error: ", e)
      rv$upload_msg = "Failed to load sites from csv, please try again."
    })
  }) %>% bindEvent(input$sites_csv)

  ### Handle test site load ----
  # observe({
  #   rv$sites <- load_sites("example-sites.csv")
  #   fit_sites()
  # }) %>% bindEvent(input$load_example)

  ### Handle clear_sites button ----
  observeEvent(input$clear_sites, {
    shinyalert(
      text = "Are you sure you want to delete all your sites?",
      type = "warning",
      closeOnClickOutside = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "Yes",
      confirmButtonCol = "#008bb6",
      cancelButtonText = "Cancel",
      callbackR = function(confirmed) {
        if (confirmed) {
          rv$sites <- sites_template
          rv$selected_site <- 1
          delete_cookie()
        }
      }
    )
  })

  ### Handle export_sites button ----
  output$export_sites <- downloadHandler(
    "sites.csv", function(file) {
      sites <- rv$sites
      req(nrow(sites) > 0)
      sites %>%
        select(id, name, lat, lng) %>%
        write_csv(file, na = "")
    }
  )


  ## Date selector ----

  ### date_ui // renderUI ----
  output$date_ui <- renderUI({
    tagList(
      uiOutput("date_select_ui"),
      uiOutput("date_btns_ui")
    )
  })

  ### date_select_ui // renderUI ----
  output$date_select_ui <- renderUI({
    div(
      dateInput(
        inputId = "start_date",
        label = "Start date:",
        min = OPTS$earliest_date,
        max = today(),
        value = OPTS$default_start_date,
        width = "100%"
      ),
      dateInput(
        inputId = "end_date",
        label = "End date:",
        min = OPTS$earliest_date,
        max = today(),
        value = today(),
        width = "100%"
      )
    )
  })

  ### date_btns_ui // renderUI ----
  output$date_btns_ui <- renderUI({
    btn <- function(id, label) actionButton(id, label, class = "btn btn-sm")
    div(
      class = "flex-across",
      btn("date_last_year", "Last year"),
      btn("date_this_year", "This year"),
      btn("date_past_month", "Past month")
    )
  })

  ### Handle date_last_year button ----
  observeEvent(input$date_last_year, {
    yr <- year(today()) - 1
    updateDateInput(inputId = "start_date", value = make_date(yr, 1, 1))
    updateDateInput(inputId = "end_date", value = make_date(yr, 12, 31))
  })

  ### Handle date_this_year button ----
  observeEvent(input$date_this_year, {
    yr <- year(today())
    updateDateInput(inputId = "start_date", value = make_date(yr, 1, 1))
    updateDateInput(
      inputId = "end_date",
      value = min(make_date(yr, 12, 31), today())
    )
  })

  ### Handle date_past_month button ----
  observeEvent(input$date_past_month, {
    updateDateInput(inputId = "start_date", value = today() - 30)
    updateDateInput(inputId = "end_date", value = today())
  })


  ## Fetch weather button ----

  ### need_weather // reactive ----
  need_weather <- reactive({
    wx <- rv$weather
    if (is.null(wx)) return(TRUE)
    if (nrow(wx) == 0) return(TRUE)
    # dates <-
    # if (nrow(selected_weather()) == 0) return(TRUE)
    sites <- sites_with_status()
    if (anyNA(sites$grid_id)) return(TRUE)
    if (any(sites$needs_download)) return(TRUE)
    FALSE
  })

  ### action_ui // renderUI ----
  output$action_ui <- renderUI({
    btn <- function(msg, ...) actionButton("fetch", msg, ...)
    rv$action_nonce
    sites <- rv$sites

    opts <- lst(
      start_date = req(input$start_date),
      end_date = req(input$end_date),
      dates_valid = as_date(start_date) <= as_date(end_date),
      need_weather = need_weather()
    )

    # control button appearance
    elem <- if (nrow(sites) == 0) {
      btn("No sites selected", disabled = TRUE)
    } else if (!opts$dates_valid) {
      btn("Invalid date selection", disabled = TRUE)
    } else if (opts$need_weather) {
      btn("Fetch weather")
    } else {
      btn("Everything up to date", class = "btn-primary", disabled = TRUE)
    }

    div(class = "submit-btn", elem)
  })

  ### status_ui // renderUI ----
  # reports to user if there's a problem with weather fetching
  output$status_ui <- renderUI({
    msg <- req(rv$status_msg)
    div(class = msg$class, style = "margin-top: 5px; padding: 10px;", msg$text)
  })

  ### Handle fetching ----
  observeEvent(input$fetch, {
    sites <- rv$sites
    date_range <- selected_dates()
    disable("fetch")
    runjs("$('#fetch').html('Downloading weather...')")

    # for each site download necessary weather data
    withProgress(
      message = "Downloading weather...",
      value = 0, min = 0, max = nrow(sites),
      {
        status <- fetch_weather(sites, date_range$start, date_range$end)
        if (status != "ok") {
          rv$status_msg <- list(
            class = "shiny-output-error",
            text = status
          )
        }
      }
    )
    rv$action_nonce <- runif(1)
    rv$weather <- saved_weather
  })



  # Map UI ----

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
    #' @param map leaflet map to add basemaps
    add_basemaps <- function(map) {
      basemaps <- OPTS$map_tiles
      for (name in names(basemaps)) {
        map <- addProviderTiles(map, basemaps[[name]], group = name)
      }
      map
    }
    btn_js <- function(id) {
      JS(paste0("(btn, map) => { Shiny.setInputValue('map_btn', '", id, "', {priority: 'event'}); }"))
    }
    btn1 <- easyButton(
      title = "Get my location",
      icon = "fa-location",
      position = "topleft",
      onClick = btn_js("user_loc")
    )
    btn2 <- easyButton(
      title = "Fit all sites on the map",
      icon = "fa-expand",
      position = "topleft",
      onClick = btn_js("zoom_sites")
    )
    btn3 <- easyButton(
      title = "Show full map",
      icon = "fa-globe",
      position = "topleft",
      onClick = btn_js("zoom_extent")
    )

    leaflet(options = leafletOptions(preferCanvas = T)) %>%
      add_basemaps() %>%
      fit_bounds(OPTS$map_bounds_wi) %>%
      addMapPane("extent", 400) %>%
      addMapPane("counties", 410) %>%
      addMapPane("grid", 420) %>%
      addMapPane("sites", 430) %>%
      addLayersControl(
        baseGroups = names(OPTS$map_tiles),
        overlayGroups = unlist(OPTS$map_layers, use.names = F),
        options = layersControlOptions(collapsed = T)
      ) %>%
      addEasyButtonBar(btn1, btn2, btn3) %>%
      # assign leaflet map object to global var 'map'
      htmlwidgets::onRender("() => { map = this; }") %>%
      suspendScroll(
        sleepTime = 0,
        wakeTime = 1000,
        hoverToWake = F,
        sleepNote = F,
        sleepOpacity = 1
      ) %>%
      addPolygons(
        data = service_bounds,
        color = "black",
        weight = 2,
        fill = FALSE,
        options = pathOptions(pane = "extent", interactive = FALSE)
      )
  })

  ## Add counties to map ----
  # observe({
  #   delay(100, {
  #     leafletProxy("map") %>%
  #       addPolygons(
  #         data = counties_sf,
  #         group = OPTS$map_layers$counties,
  #         label = ~paste0("<b>", state_name, "</b></br>", county_name, " County") %>%
  #           lapply(HTML),
  #         color = "black", weight = .2, opacity = .2,
  #         fillColor = ~colorFactor(OPTS$state_colors, state_name)(state_name),
  #         fillOpacity = .1,
  #         options = pathOptions(pane = "counties")
  #       )
  #   })
  # })

  ## searchbox_ui // renderUI ----
  output$searchbox_ui <- renderUI({
    div(
      HTML(paste0("<script async src='https://maps.googleapis.com/maps/api/js?key=", OPTS$google_key, "&loading=async&libraries=places&callback=initAutocomplete'></script>")),
      # textInput("searchbox", "Find a location by name")
      textInput("searchbox", NULL)
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
      style = "display: flex; flex-direction: column;",
      # div(tags$label("Find a location by coordinates")),
      div(
        style = "display: inline-flex; gap: 5px;",
        div(
          style = "flex: 1;",
          textInput(
            inputId = "coord_search",
            label = NULL,
            placeholder = "Enter coordinates"
          )
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
    wx <- rv$weather
    sites <- rv$sites
    map %>% clearGroup("sites")
    req(nrow(sites) > 0)

    # determine site icons
    sites <- if (nrow(wx) == 0) {
      rv$sites %>% mutate(needs_download = TRUE)
    } else {
      sites_with_status()
    }

    sites <- sites %>%
      mutate(
        icon = case_when(
          temp ~ "thumbtack",
          needs_download ~ "download",
          !temp ~ as.character(id),
          TRUE ~ "check"
        ),
        marker_color = if_else(id == rv$selected_site, "red", "blue"),
        label = paste0(
          "<b>Site ", id, ": ", name,
          if_else((nrow(sites) > 1) & id == rv$selected_site, " [Selected]", ""),
          "</b><br>",
          sprintf("%.3f°N, %.3f°W", lat, lng),
          "<br>", if_else(needs_download, "Download required", "Data ready"),
          "<br>", if_else(temp, "Click to pin site", "Pinned site")
        ) %>% lapply(HTML)
      )

    leafletProxy("map") %>%
      addAwesomeMarkers(
        data = sites,
        lat = ~lat,
        lng = ~lng,
        label = ~label,
        layerId = ~id,
        group = "sites",
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
    map <- leafletProxy("map")
    grids <- grids_with_status()

    if (nrow(grids) > 0) {
      grids <- grids %>%
        mutate(
          title = if_else(needs_download, "Incomplete weather grid", "Downloaded weather grid"),
          color = if_else(needs_download, "orange", "blue"),
          label = paste0(
            "<b>", title, "</b><br>",
            "Earliest date: ", date_min, "<br>",
            "Latest date: ", date_max, "<br>",
            "Total days: ", days_expected, "<br>",
            "Missing days: ", days_missing, sprintf(" (%.1f%%)", 100 * days_missing_pct), "<br>",
            "Missing hours: ", hours_missing, sprintf(" (%.1f%%)", 100 * hours_missing_pct), "<br>",
            "Center latitude: ", sprintf("%.2f", grid_lat), "<br>",
            "Center longitude: ", sprintf("%.2f", grid_lng)
          ) %>% lapply(HTML)
        )
      map %>%
        addPolygons(
          data = grids,
          weight = 1,
          label = ~label,
          layerId = ~grid_id,
          group = "grid",
          fillColor = ~color,
          options = pathOptions(pane = "grid")
        )
    } else {
      clearGroup(map, "grid")
    }

  })


  # Map handlers ----

  fly_to <- function(loc) {
    leafletProxy("map") %>%
      flyTo(loc$lng, loc$lat, max(10, isolate(input$map_zoom)))
  }

  fit_sites <- function() {
    sites <- rv$sites
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
    runjs(str_glue("getLocalityName({loc$lat}, {loc$lng}, '{OPTS$google_key}')"))
  }) %>% bindEvent(input$user_loc)

  ## Handle location from click ----
  observe({
    loc <- req(input$map_click)
    runjs(str_glue("getLocalityName({loc$lat}, {loc$lng}, '{OPTS$google_key}')"))
  }) %>% bindEvent(input$map_click$.nonce)

  ## Save site after getting locality name from geocoding api
  observe({
    loc <- req(input$locality_name)
    site <- create_site(loc)
    save_site(site)
    fly_to(site)
    runjs("sendShiny('locality_name', null)")
  })

  ## Handle marker click ----
  observe({
    marker <- req(input$map_marker_click)
    id <- marker$id
    sites <- rv$sites
    site <- sites[id,]
    if (rv$selected_site != id) rv$selected_site <- id
    if (site$temp == TRUE) sites$temp[id] <- FALSE
    rv$sites <- sites
    # fly_to(marker)
  }) %>%
    bindEvent(input$map_marker_click$.nonce)



  # Data tab ----

  dataServer(
    wx_data = reactive(wx_data()),
    selected_site = reactive(rv$selected_site),
    sites_ready = reactive(rv$sites_ready),
    weather_ready = reactive(rv$weather_ready)
  )


  # Disease models tab ----

  riskServer(
    wx_data = reactive(wx_data()),
    selected_site = reactive(rv$selected_site),
    sites_ready = reactive(rv$sites_ready),
    weather_ready = reactive(rv$weather_ready)
  )

}
