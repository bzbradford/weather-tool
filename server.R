
server <- function(input, output, session) {

  sites_template <- tibble(name = character(), lat = numeric(), lng = numeric())

  rv <- reactiveValues(
    sites = NULL,
    site_located = NULL,
    site_clicked = NULL,
    site_searched = NULL,
    show_upload = FALSE
  )

  temp_sites <- reactive({
    sites_template %>%
      bind_rows(as_tibble(rv$site_located)) %>%
      bind_rows(as_tibble(rv$site_clicked)) %>%
      bind_rows(as_tibble(rv$site_searched))
  })

  saved_sites <- reactive({
    sites_template %>%
      bind_rows(as_tibble(rv$sites))
  })



  # Sidebar -----------------------------------------------------------------

  output$sites_ui <- renderUI({
    tagList(
      h4("Temporary sites"),
      uiOutput("temp_sites_ui"),
      hr(),
      h4("Site list"),
      uiOutput("saved_sites_ui"),
      p(
        actionButton("load_example", "Test sites"),
        actionButton("upload_csv", "Upload csv"),
        actionButton("clear_sites", "Clear sites")
      ),
      uiOutput("file_upload_ui"),
      hr(),
    )
  })

  output$temp_sites_ui <- renderUI({
    temp_sites <- temp_sites()
    validate(need(nrow(temp_sites) > 0, "Click or search for a new location"))
    df <- temp_sites %>%
      mutate(action = "Save") %>%
      clean_names("title")
    tagList(
      em("Weather data will not be fetched for these locations until they are saved to the list"),
      renderTable(df)
    )

  })

  output$saved_sites_ui <- renderUI({
    sites <- rv$sites
    validate(need(sites, "No sites in list"))
    df <- sites %>%
      select(name, lat, lng) %>%
      mutate(data = "0%") %>%
      clean_names("title")
    tagList(
      em("Set a start and end date below then click 'Fetch weather' to load weather data."),
      renderTable(df)
    )
  })

  output$file_upload_ui <- renderUI({
    req(rv$show_upload)
    div(
      tags$label("Upload csv"), br(),
      em("Must have columns 'name', 'lat', 'lng'"),
      fileInput(
        inputId = "sites_csv",
        label = NULL,
        accept = ".csv"
      )
    )
  })

  observe({
    upload <- req(input$sites_csv)
    col_names <- c(
      name = "name",
      name = "location",
      lat = "latitude",
      lng = "longitude",
      lng = "long"
    )
    try({
      upload <- upload$datapath %>%
        read_csv(show_col_types = F) %>%
        janitor::clean_names() %>%
        select(any_of(col_names)) %>%
        drop_na()
      req(nrow(upload) > 0)
      req(c("name", "lat", "lng") %in% names(upload))
      rv$sites <- upload
      rv$show_upload <- FALSE
    })
  })

  observeEvent(input$load_example, {
    rv$sites <- test_sites
  })

  observeEvent(input$upload_csv, {
    rv$show_upload <- !rv$show_upload
  })

  observeEvent(input$clear_sites, {
    rv$sites <- NULL
  })

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


  output$action_ui <- renderUI({
    actionButton("get", "Fetch weather", width = "100%")
  })




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
    flyTo(leafletProxy("map"), loc$lng, loc$lat, 10)
  }

  add_marker <- function(loc, icon, layer_id) {
    leafletProxy("map") %>%
      addAwesomeMarkers(
        lat = loc$lat,
        lng = loc$lng,
        label = sprintf("%s: %.2f, %.2f", loc$name, loc$lat, loc$lng),
        layerId = layer_id,
        icon = makeAwesomeIcon(icon = icon),
        options = pathOptions(pane = "sites")
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

  ## Render saved sites ----
  # TODO: use different icons to indicate status of each site
  observe({
    map <- leafletProxy("map")
    sites <- rv$sites
    if (is.null(sites)) return(clearGroup(map, "sites"))
    addAwesomeMarkers(
      map,
      data = sites,
      lat = ~lat,
      lng = ~lng,
      label = ~sprintf("%s (%.2f, %.2f)", name, lat, lng),
      group = "sites",
      icon = makeAwesomeIcon(icon = "download"),
      options = pathOptions(pane = "sites")
    )
  })

  ## Render temporary locations ----
  observe({
    loc <- req(rv$site_clicked)
    add_marker(loc, "plus", "clicked")
  })

  observe({
    loc <- req(rv$site_searched)
    print(loc)
    add_marker(loc, "search", "searched") %>%
      flyTo(lat = loc$lat, lng = loc$lng, zoom = 10)
  })

  observe({
    loc <- req(rv$site_located)
    add_marker(loc, "home", "home") %>%
      flyTo(lat = loc$lat, lng = loc$lng, zoom = 10)
  })


  # Map button handler ----

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
      sites <- bind_rows(temp_sites(), saved_sites())
      req(nrow(sites) > 0)
      bounds <- list(
        lat1 = min(sites$lat),
        lat2 = max(sites$lat),
        lng1 = min(sites$lng),
        lng2 = max(sites$lng)
      )
      fit_bounds(bounds = bounds, options = list(padding = c(100, 100), maxZoom = 10))
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
  observe({
    loc <- req(input$searched_loc)

    if (validate_loc(loc)) {
      rv$site_searched <- loc
      runjs("
        document.getElementById('searchbox').value = '';
        document.getElementById('coord_search').value = '';
      ")
    }
  })

  ## Handle location from click ----
  observe({
    loc <- req(input$map_click)
    if (validate_loc(loc)) {
      rv$site_clicked <- list(
        name = "Clicked point",
        lat = loc$lat,
        lng = loc$lng
      )
    }
  })

  ## Handle geolocation ----
  observe({
    loc <- req(input$user_loc)
    if (validate_loc(loc)) {
      rv$site_located <- list(
        name = "Geolocated",
        lat = loc$lat,
        lng = loc$lng
      )
    }
  })
}
