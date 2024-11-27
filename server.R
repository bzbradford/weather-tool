
server <- function(input, output, session) {

  # try to enforce site attributes
  create_site <- function(loc) {
    sites <- rv$sites
    loc$id <- ifelse(nrow(sites) > 0, max(sites$id) + 1, 1)
    if (is.null(loc$group)) loc$group = "temp"

    # make sure it has all the attributes
    stopifnot(all(names(sites_template) %in% names(loc)))
    loc <- loc[names(loc) %in% names(sites_template)]
    loc$lat <- round(loc$lat, 4)
    loc$lng <- round(loc$lng, 4)
    req(validate_loc(loc))
    loc
  }

  save_temp_site <- function() {
    site <- rv$temp_site
    site$group = "sites"
    site$icon = "download"
    rv$sites <- bind_rows(
      rv$sites,
      as_tibble(site)
    ) %>%
      distinct(name, lat, lng, .keep_all = T) %>%
      mutate(id = row_number())

    rv$temp_site <- NULL
  }

  rv <- reactiveValues(
    weather = NULL,
    temp_site = NULL, # list
    sites = sites_template, # df
    selected_site = 1,

    show_upload = FALSE,
    upload_msg = NULL
  )

  observe(print(rv$temp_site))

  sites_df <- reactive({
    if (input$multi_site)
      bind_rows(sites_template, rv$sites, as_tibble(rv$temp_site))
    else
      bind_rows(sites_template, as_tibble(rv$temp_site))
  })

  sites_sf <- reactive({
    sites <- sites_df()
    req(nrow(sites) > 0)
    sites %>%
      select(id, name, lat, lng) %>%
      st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = F)
  })

  observe(print(sites_sf()))



  # Sidebar -----------------------------------------------------------------

  ## site_ui ----
  output$site_ui <- renderUI({
    if (input$multi_site)
      uiOutput("multi_site_ui")
    else
      uiOutput("single_site_ui")
  })

  ## single_site_ui ----

  output$single_site_ui <- renderUI({
    div(style = "overflow:auto;", tableOutput("sites_tbl"))
  })

  ## multi_site_ui ----
  output$multi_site_ui <- renderUI({
    div(
      p(em("Load or queue up multiple sites. A clicked or searched location must be clicked again to save it to the list.")),
      tags$label("Temporary site:"),
      div(style = "overflow:auto;", tableOutput("temp_tbl")),
      tags$label("Saved sites:"),
      div(style = "overflow:auto;", tableOutput("sites_tbl")),
      p(
        actionButton("load_example", "Test sites", class = "btn-small"),
        actionButton("upload_csv", "Upload csv"),
        actionButton("clear_sites", "Clear sites")
      ),
      uiOutput("file_upload_ui"),
    )
  })

  ## temp_tbl ----
  output$temp_tbl <- renderTable({
    site <- req(rv$temp_site)
    as_tibble(site) %>%
      select(id, name, lat, lng) %>%
      clean_names("title")
  })

  ## sites_tbl ----
  output$sites_tbl <- renderTable({
    sites <- sites_df()
    if (input$multi_site) {
      sites <- sites %>% filter(group != "temp")
    }
    validate(need(nrow(sites) > 0, "No sites in list. Click or search on the map or use the buttons below to upload a list of sites."))
    sites %>%
      select(id, name, lat, lng) %>%

      # TODO: need to look at weather data for this value
      # mutate(data = "0%") %>%
      clean_names("title")
  })

  ## file_upload_ui ----
  output$file_upload_ui <- renderUI({
    req(rv$show_upload)
    div(
      tags$label("Upload csv"), br(),
      em("Must have columns 'name', 'lat', 'lng'"),
      fileInput(
        inputId = "sites_csv",
        label = NULL,
        accept = ".csv"
      ),
      { if (!is.null(rv$upload_msg)) div(class = "shiny-error", rv$upload_msg) }
    )
  })

  # handle sites upload
  observe({
    upload <- req(input$sites_csv)
    tryCatch({
      upload <- upload$datapath %>%
        read_csv(show_col_types = F) %>%
        load_sites()
      rv$sites <- upload
      rv$selected_site <- 1
      rv$show_upload <- FALSE
      rv$upload_msg <- NULL
    }, error = function(e) {
      rv$upload_msg = "Failed to load sites from csv, please try again."
    })
  })

  observe({
    rv$sites <- read_csv("example-sites.csv", show_col_types = F) %>%
      load_sites()
    fit_sites()
  }) %>% bindEvent(input$load_example)

  observe({
    rv$show_upload <- !rv$show_upload
  }) %>% bindEvent(input$upload_csv)

  observe({
    rv$temp_site <- NULL
    rv$sites <- sites_template
    rv$selected_site <- 1
  }) %>% bindEvent(input$clear_sites)


  ## date_ui ----
  output$date_ui <- renderUI({
    opts <- list(
      min = Sys.Date() - 30,
      max = Sys.Date()
    )
    div(
      dateInput(
        inputId = "start_date",
        label = "Start date:",
        min = opts$min,
        max = opts$max,
        value = opts$min
      ),
      dateInput(
        inputId = "end_date",
        label = "End date:",
        min = opts$min,
        max = opts$max,
        value = opts$max
      )
    )
  })


  ## action_ui ----
  output$action_ui <- renderUI({
    btn <- function(msg, ...) actionButton("get", msg, width = "100%", ...)
    sites <- sites_df()
    if (nrow(sites) == 0) {
      btn("No sites selected", disabled = TRUE)
    } else {
      btn("Fetch weather")
    }
  })

  observe({
    sites <- sites_df()
    wx <- as_tibble(rv$weather)
    opts <- list(
      start_date = req(input$start_date),
      end_date = req(input$end_date)
    )
    disable("get")
    for (i in 1:nrow(sites)) {
      site <- sites[i,]
      print(site)
      wx <- bind_rows(wx, get_ibm(site$lat, site$lng, opts$start_date, opts$end_date))
    }
    rv$weather <- wx %>%
      distinct(requestedLatitude, requestedLongitude, validTimeUtc, .keep_all = T)
    enable("get")
  }) %>% bindEvent(input$get)



  # Map ---------------------------------------------------------------------

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

  fly_to <- function(loc) {
    leafletProxy("map") %>%
      flyTo(loc$lng, loc$lat, max(10, isolate(input$map_zoom)))
  }

  add_marker <- function(loc, icon, layer_id) {
    leafletProxy("map") %>%
      addAwesomeMarkers(
        lat = loc$lat,
        lng = loc$lng,
        label = sprintf("%s: %.2f, %.2f", loc$name, loc$lat, loc$lng),
        layerId = layer_id,
        icon = makeAwesomeIcon(icon = icon),
        options = markerOptions(pane = "sites")
      )
  }


  ## Initialize map ----

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
        overlayGroups = unlist(OPTS$map_layers, use.names = F),
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


  ## Show site markers ----
  observe({
    map <- leafletProxy("map")

    clearGroup(map, "sites")
    clearGroup(map, "temp")

    sites <- sites_df()
    req(nrow(sites) > 0)
    leafletProxy("map") %>%
      addAwesomeMarkers(
        data = sites,
        lat = ~lat,
        lng = ~lng,
        label = ~sprintf("%s (%.2f, %.2f)", name, lat, lng),
        layerId = ~id,
        group = ~group,
        icon = ~makeAwesomeIcon(icon = icon),
        options = pathOptions(pane = "sites")
      )
  })


  ## Show weather data grids ----
  observe({
    leafletProxy("map") %>%
      addPolygons(
        data = wx_grids(),
        weight = 1,
        label = "Weather grid",
        layerId = ~grid_id,
        options = pathOptions(pane = "grid")
      )
  })



  ## Map button handler ----

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

  observeEvent(input$map_btn, {
    btn <- req(input$map_btn)
    map <- leafletProxy("map")

    if (btn == "user_loc") {
      runjs("
        map.getMap().locate({ setView: false }).on('locationfound', (event) => {
          Shiny.setInputValue('user_loc', event.latlng, {priority: 'event'})
        })
      ")
    } else if (btn == "zoom_sites") {
      fit_sites()
    } else if (btn == "zoom_extent") {
      fit_bounds(bounds = OPTS$map_bounds_us)
    }
  })


  # Search box ----

  output$searchbox_ui <- renderUI({
    div(
      HTML(paste0("<script async src='https://maps.googleapis.com/maps/api/js?key=", OPTS$google_key, "&loading=async&libraries=places&callback=initAutocomplete'></script>")),
      textInput("searchbox", "Find a location by name")
    )
  })


  ## Coordinate search ----
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

  observeEvent(input$coord_search_go, {
    str <- req(input$coord_search)
    try({
      coords <- parse_coords(str)
      coord_hash <- paste0("{name: 'Searched', lat:", coords$lat, ", lng:", coords$lng, "}")
      cmd <- paste0("Shiny.setInputValue('searched_loc', ", coord_hash, ", {priority: 'event'})")
      runjs(cmd)
    })
  })


  ## Handle searched location from google or coordinates ----
  # arrives with a name attribute already
  observe({
    loc <- req(input$searched_loc)
    loc$icon <- "search"
    rv$temp_site <- create_site(loc)
    runjs("
      document.getElementById('searchbox').value = '';
      document.getElementById('coord_search').value = '';
    ")
    fly_to(loc)
  })

  ## Handle geolocation ----
  observe({
    loc <- req(input$user_loc)
    loc$name <- "Geolocated"
    loc$icon <- "home"
    rv$temp_site <- create_site(loc)
    fly_to(loc)
  })

  ## Handle location from click ----
  observe({
    loc <- req(input$map_click)
    message('click')
    print(loc)
    loc$name <- "Clicked point"
    loc$icon <- "plus"
    rv$temp_site <- create_site(loc)
    fly_to(loc)
  }) %>%
    bindEvent(input$map_click$.nonce)

  ## Handle marker click ----
  observe({
    marker <- req(input$map_marker_click)
    message("marker")
    print(marker)
    if (input$multi_site & marker$group == "temp") {
      site <- rv$temp_site
      rv$temp_site <- NULL
      site$group = "sites"
      site$icon = "download"
      rv$sites <-
        bind_rows(rv$sites, as_tibble(site)) %>%
        mutate(id = row_number())
    }
    rv$selected_site <- marker$id
    fly_to(marker)
  }) %>%
    bindEvent(input$map_marker_click$.nonce)



  # Weather data ---------------------------------------------------------------

  wx_grids <- reactive({
    build_grids(req(rv$weather)) %>%
      rowwise() %>%
      mutate(geometry = ll_to_grid(grid_lat, grid_lng)) %>%
      ungroup() %>%
      st_set_geometry("geometry")
  })

  sites_with_grid <- reactive({
    sites_sf() %>%
      st_join(wx_grids()) %>%
      st_set_geometry(NULL)
  })

  wx_raw <- reactive({
    req(rv$weather)
  })

  wx_raw_joined <- reactive({
    sites_with_grid() %>%
      left_join(wx_raw(), join_by(grid_id == gridpointId))
  })

  wx_hourly <- reactive({
    build_hourly(req(rv$weather))
  })

  # hourly already has some duplicate grid attributes
  wx_hourly_joined <- reactive({
    sites_with_grid() %>%
      select(id, name, lat, lng, grid_id) %>%
      left_join(wx_hourly(), join_by(grid_id))
  })

  wx_daily <- reactive({
    build_daily(wx_hourly())
  })

  wx_daily_joined <- reactive({
    sites_with_grid() %>%
      left_join(wx_daily(), join_by(grid_id))
  })

  output$data_ui <- renderUI({
    validate(need(rv$weather, "No weather data downloaded yet."))
    tabsetPanel(
      tabPanel(
        title = "Raw data",
        p(em("Hourly weather data retrieved directly from the IBM weather service.")),
        div(style = "overflow:auto;", DTOutput("data_raw"))
      ),
      tabPanel(
        title = "Hourly",
        p(em("Selected hourly weather parameters and additional generated data columns.")),
        div(style = "overflow:auto;", DTOutput("data_hourly"))
      ),
      tabPanel(
        title = "Daily",
        p(em("Daily weather summary and moving averages, generated from hourly weather data.")),
        div(style = "overflow:auto;", DTOutput("data_daily"))
      ),
      tabPanel(
        title = "Sites/Grids",
        p(em("Sites and corresponding data grids.")),
        div(style = "overflow:auto;", DTOutput("data_grids"))
      ),
      tabPanel(
        title = "Info",
        p(em("Metadata and column definitions")),
        gt_output("data_defs")
      )
    )
  })

  output$data_raw <- renderDT({
    wx_raw_joined() %>%
      clean_names("big_camel")
  })

  output$data_hourly <- renderDT({
    wx_hourly_joined() %>%
      clean_names("big_camel")
  })

  output$data_daily <- renderDT({
    wx_daily_joined() %>%
      clean_names("big_camel")
  })

  output$data_grids <- renderDT({
    d <- OPTS$grid_dim
    sites_with_grid() %>%
      mutate(
        lat_min = grid_lat - d,
        lat_max = grid_lat + d,
        lng_min = grid_lng - d,
        lng_max = grid_lng + d
      ) %>%
      clean_names("big_camel")
  })

  output$data_defs <- render_gt({
    ibm_cols %>%
      clean_names("big_camel")
  })


}


