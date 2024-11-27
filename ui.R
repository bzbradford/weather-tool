#-- ui.R --#

sidebarUI <- function() {
  div(
    materialSwitch("multi_site", "Multi site mode"),
    uiOutput("site_ui"),
    uiOutput("date_ui"),
    uiOutput("action_ui")
  )
}

mapUI <- function() {
  div(
    div(
      style = "margin-bottom: 10px;",
      leafletOutput("map", height = "600px"),
    ),
    fluidRow(
      column(6, uiOutput("searchbox_ui")),
      column(6, uiOutput("coord_search_ui"))
    )
  )
}

ui <- dashboardPage(
  header = dashboardHeader(
    title = "IBM Weather Data Portal"
  ),
  sidebar = dashboardSidebar(disable = T),
  body = dashboardBody(
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$meta(name = "description", content = "A tool for downloading hourly weather data for any location in the continental United States"),
      tags$meta(name = "keywords", content = "uw, wisconsin, weather, tool"),
      # tags$link(rel = "shortcut icon", href = "favicon.ico"),
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$script(src = "script.js"),
      # includeHTML("www/google-analytics.html"),
      useShinyjs()
    ),
    fluidRow(
      box(
        title = "Sites",
        width = 2,
        solidHeader = T,
        collapsible = T,
        status = "primary",
        sidebarUI()
      ),
      box(
        title = "Map",
        width = 5,
        collapsible = T,
        status = "primary",
        mapUI()
      ),
      box(
        title = "Weather data",
        width = 5,
        status = "primary",
        uiOutput("data_ui")
      )
    )
  )
)

# ui <- page_fillable(
#   title = "IBM Weather Data Portal",
#   # theme = shinytheme("flatly"),
#   tags$head(
#     tags$meta(charset = "UTF-8"),
#     tags$meta(name = "description", content = "A tool for downloading hourly weather data for any location in the continental United States"),
#     tags$meta(name = "keywords", content = "uw, wisconsin, weather, tool"),
#     # tags$link(rel = "shortcut icon", href = "favicon.ico"),
#     tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
#     tags$script(src = "script.js"),
#     # includeHTML("www/google-analytics.html"),
#     useShinyjs()
#   ),
#   padding = 0,
#   h4("Weather portal"),
#   layout_sidebar(
#     sidebar = sidebarUI(),
#     column(6, card(mapUI())),
#     column(6,
#       tabsetPanel(
#         tabPanel("Plot", uiOutput("plots")),
#         tabPanel("Table", uiOutput("table"))
#       )
#     )
#
#
#   )
# )

