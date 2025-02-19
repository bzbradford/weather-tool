#-- ui.R --#

ui <- fluidPage(
  title = OPTS$app_title,
  theme = shinytheme("flatly"),
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "description", content = "A tool for downloading hourly weather data for any location in the continental United States"),
    tags$meta(name = "keywords", content = "uw, wisconsin, weather, tool"),
    tags$link(rel = "preload", href = "https://cdn.wisc.cloud/fonts/uw-rh/0.0.1/redhat-display-latin.v14.woff2", as = "font", type = "font/woff2"),
    tags$link(rel = "preload", href = "https://cdn.wisc.cloud/fonts/uw-rh/0.0.1/redhat-text-latin.v13.woff2", as = "font", type = "font/woff2"),
    tags$link(rel = "shortcut icon", href = "uw-crest.svg"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(src = "script.js"),
    includeHTML("www/google-analytics.html"),
    useShinyjs()
  ),
  tags$header(
    div(class = "uw-title",
      # img(src = "crop-protection-network-logo.png"),
      img(src = "uw-crest.svg"),
      h1(OPTS$app_title)
    ),
    div(class = "help-btn", actionLink("help", icon("circle-info")))
  ),
  div(class = "main-container",
    div(class = "column sidebar-container",
      h2("Site selection"),
      div(
        div(
          style = "margin-top: 10px;",
          materialSwitch("multi_site", "Multi site mode")
        ),
        div(
          style = "margin-top: -10px;",
          uiOutput("site_ui"),
          uiOutput("multi_site_ui"),
        ),
        div(
          style = "margin-top: 20px;",
          uiOutput("date_ui"),
        ),
        div(
          style = "margin-top: 20px;",
          uiOutput("action_ui"),
          uiOutput("status_ui")
        )
      )
    ),
    div(class = "column map-container",
      leafletOutput("map", height = "100%"),
      div(class = "search-overlay",
        uiOutput("searchbox_ui"),
        uiOutput("coord_search_ui")
      )
    ),
    div(class = "column data-container",
      tabsetPanel(
        tabPanel("View data", dataUI()),
        tabPanel("Disease risk", riskUI())
      )
    )
  ),
  tags$footer(
    div(class = "footer",
      "Developed by",
      a("Ben Bradford", href = "https://entomology.wisc.edu/directory/ben-bradford/", target = "_blank", .noWS = "after"),
      ", UW-Madison Entomology",
      br(),
      a("View source code", href = "https://github.com/bzbradford/weather-tool", target = "_blank", .noWS = "after"),
    )
  )
)

