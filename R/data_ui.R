
dataUI <- function() {
  ns <- NS("data")
  uiOutput(ns("main_ui"))
}

dataServer <- function(sites_df, selected_site, wx_data) {
  moduleServer(
    id = "data",
    function(input, output, session) {
      ns <- session$ns

      rv <- reactiveValues(
        # both must be true to show data display
        sites_ready = FALSE,
        weather_ready = FALSE,
      )

      ## selected_data // reactive ----
      selected_data <- reactive({
        opts <- list()
        opts$data_type <- req(input$data_type)
        if (opts$data_type == "ma") {
          opts$ma_align <- req(input$ma_align)
        }
        wx <- wx_data()

        sites <- wx$sites %>%
          st_drop_geometry() %>%
          select(
            site_id = id,
            site_name = name,
            site_lat = lat,
            site_lng = lng,
            grid_id
          )

        data <- switch(
          opts$data_type,
          "hourly" = wx$hourly,
          "daily" = wx$daily,
          "ma" = switch(
            opts$ma_align,
            "center" = wx$ma_center,
            "right" = wx$ma_right
          ),
          "disease" = wx$disease,
          "gdd" = wx$gdd
        )

        df <- sites %>%
          left_join(data, join_by(grid_id), relationship = "many-to-many") %>%
          drop_na(grid_id, date) %>%
          select(-grid_id) %>%
          mutate(across(where(is.numeric), ~signif(.x)))

        if (input$metric) df else convert_measures(df)
      })



      ## download_data // reactive ----
      download_data <- reactive({
        selected_data() %>%
          rename_with_units() %>%
          mutate(across(any_of(c("datetime_utc", "datetime_local")), as.character)) %>%
          clean_names("big_camel")
      })

      ## download_filename // reactive ----
      # for both the csv download and plot png export
      download_filename <- reactive({
        type <- req(input$data_type)
        wx <- wx_data()
        sites <- wx$sites
        dates <- wx$dates
        name_str <- invert(OPTS$data_type_choices)[[type]]
        site_str <- ifelse(
          nrow(sites) == 1,
          sprintf("(%.3f, %.3f)", sites$lat, sites$lng),
          "(multiple locations)"
        )
        date_str <- paste(dates$start, "to", dates$end)
        list(
          csv = paste(name_str, "data", site_str, "-", date_str),
          plot = paste(name_str, "plot", site_str, "-", date_str)
        )
      })

      ## control if the data view is ready ----
      observe({
        sr <- nrow(sites_df()) > 0
        if (rv$sites_ready != sr) rv$sites_ready <- sr
      })

      observe({
        wr <- nrow(wx_data()$hourly) > 0
        if (rv$weather_ready != wr) rv$weather_ready <- wr
      })

      ## main_ui // renderUI ----
      output$main_ui <- renderUI({
        tagList(
          p(em("Most values may be shown in either metric or imperial units. Temperature and dew point: °C or °F, precipitation (rain/melted snow): mm or in, snow accumulation: cm or in, relative humidity: %, pressure: mbar or inHg, wind speed: m/s or mph, wind direction: compass degrees (N=0°, E=90°, etc.). Growing degree day base/upper thresholds and accumulations always in Fahrenheit.")),
          materialSwitch(
            inputId = ns("metric"),
            label = "Use metric",
            value = TRUE,
            status = "primary"
          ),
          radioGroupButtons(
            inputId = ns("data_type"),
            label = "Dataset",
            choices = OPTS$data_type_choices
          ),
          uiOutput(ns("data_options")),
          uiOutput(ns("dataset_ui"))
        )
      })

      ## dataset_ui // renderUI ----
      # TODO: this validation still gets locked out by the reactives
      output$dataset_ui <- renderUI({
        validate(need(rv$weather_ready, "No weather data downloaded yet for the selected dates. Click 'Fetch Weather' to download."))
        validate(need(rv$sites_ready, "No sites selected, click on the map or load sites in the sidebar."))

        tagList(
          uiOutput(ns("data_msg")),
          h4("Data chart"),
          uiOutput(ns("plot_ui")),
          downloadButton(ns("download_data"), "Download dataset")
        )
      })

      ## data_options // renderUI ----
      output$data_options <- renderUI({
        type <- req(input$data_type)
        req(type == "ma")
        div(
          class = "flex-across",
          div(tags$label("Moving average type:")),
          radioButtons(
            inputId = ns("ma_align"),
            label = NULL,
            choices = c("Centered" = "center", "Trailing" = "right"),
            selected = isolate(input$ma_align),
            inline = TRUE
          )
        )
      })

      ## data_msg // renderUI ----
      output$data_msg <- renderUI({
        sites <- wx_data()$sites
        req(nrow(sites) > 0)
        req(any(sites$needs_download))
        span(style = "color: red;", "Some sites are missing data based on your date selections, click 'Fetch weather' to load missing data.")
      })

      ## plot_ui // renderUI ----
      output$plot_ui <- renderUI({
        div(
          uiOutput(ns("plot_sites_ui")),
          uiOutput(ns("plot_cols_ui")),
          div(
            class = "plotly-container",
            plotlyOutput(ns("data_plot"))
          )
        )
      })

      ## plot_sites // renderUI ----
      output$plot_sites_ui <- renderUI({
        sites <- wx_data()$sites
        req(nrow(sites) > 1)
        choices <- set_names(sites$id, sprintf("%s: %s", sites$id, str_trunc(sites$name, 15)))
        checkboxGroupInput(
          inputId = ns("plot_sites"),
          label = "Sites to display",
          choices = choices,
          selected = selected_site(),
          inline = TRUE
        )
      })

      ## plot_cols // reactive ----
      plot_cols <- reactive({
        cols <- names(selected_data())
        cols <- cols[!(cols %in% OPTS$plot_ignore_cols)]
        set_names(cols, make_clean_names(cols, "title"))
      })

      ## plot_cols // renderUI ----
      output$plot_cols_ui <- renderUI({
        cols <- plot_cols()
        prev_selection <- intersect(cols, isolate(input$plot_cols))
        default_selection <- intersect(cols, OPTS$plot_default_cols)
        div(
          div(
            class = "flex-across",
            div(
              style = "flex:1;",
              selectizeInput(
                inputId = ns("plot_cols"),
                label = NULL,
                choices = cols,
                selected = first_truthy(prev_selection, default_selection, cols[1]),
                multiple = TRUE,
                options = list(plugins = list("remove_button"))
              )
            ),
            div(
              class = "reset-plot",
              actionLink(ns("reset_plot_cols"), icon("refresh"))
            )
          )
        )
      })

      ## Reset plot columns ----
      reset_plot_cols <- function() {
        cols <- plot_cols()
        default_col <- intersect(cols, OPTS$plot_default_cols)
        updateSelectizeInput(
          inputId = "plot_cols",
          selected = first_truthy(default_col, cols[1])
        )
      }

      # reset when all columns are removed
      # observe({ if (length(input$plot_cols) == 0) reset_plot_cols() })

      # reset on button press
      observe(reset_plot_cols()) %>% bindEvent(input$reset_plot_cols)

      ## data_plot // renderPlotly ----
      output$data_plot <- renderPlotly({
        wx <- wx_data()
        sites <- wx$sites
        df <- selected_data()
        req(nrow(sites) > 0)

        opts <- lst(
          multi_site = nrow(sites) > 1,
          dates = wx$dates,
          date_range = c(
            ymd_hms(paste(dates$start, "00:00:00")),
            ymd_hms(paste(dates$end, "23:00:00"))
          ),
          data_type = req(input$data_type),
          data_name = invert(OPTS$data_type_choices)[[data_type]],
          cols = req(input$plot_cols),
          unit_system = ifelse(input$metric, "metric", "imperial"),
          site_ids = unique(df$site_id),
          filename = download_filename()$plot
        )

        if (opts$multi_site) {
          opts$selected_ids = req(input$plot_sites)
          df <- filter(df, site_id %in% opts$selected_ids)
        }
        req(nrow(df) > 0)
        req(all(opts$cols %in% names(df)))

        # change marker style depending on amount of data
        opts$mode <- ifelse(nrow(df) <= 100, "lines+markers", "lines")
        opts$linewidth <- ifelse(nrow(df) <= 500, 2, 1)

        # create plot title
        opts$title <- if (!opts$multi_site) {
          req(nrow(sites) == 1)
          sprintf("%s data for %.3f°N, %.3f°W", opts$data_name, sites$lat, sites$lng)
        } else {
          site_locs <-
            with(
              filter(sites, id %in% opts$selected_ids),
              sprintf("Site %s: %.2f,%.2f", id, lat, lng)
            ) %>%
            paste(collapse = " / ") %>%
            str_wrap(120) %>%
            str_replace_all("\\\n", "<br>")
          paste0(opts$data_name, " data<br><span style='font-size:12px;font-style:italic;'>", site_locs, "</span>")
        }

        if ("datetime_local" %in% names(df)) df$date <- df$datetime_local

        # try to assign the columns to axes with values in similar ranges
        # also bunches the more numerous columns on the left
        col_ranges <- df %>%
          summarize(across(all_of(opts$cols), ~max(.x, na.rm = T))) %>%
          pivot_longer(everything()) %>%
          drop_na(value) %>%
          mutate(value = sqrt(abs(value))) %>%
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
            title = list(
              text = opts$title,
              font = OPTS$plot_title_font
            ),
            hovermode = "x",
            showlegend = TRUE,
            margin = list(t = 50, r = 50),
            legend = list(orientation = "h"),
            xaxis = list(
              range = opts$date_range
            ),
            yaxis = list(
              title = list(
                text = str_wrap(y1_title, 40),
                font = OPTS$plot_axis_font
              )
            ),
            yaxis2 = list(
              overlaying = "y",
              side = "right",
              title = list(
                text = str_wrap(y2_title, 40),
                font = OPTS$plot_axis_font
              )
            )
          ) %>%
          config(
            displaylogo = FALSE,
            toImageButtonOptions = list(
              format = "png",
              filename = opts$filename,
              height = 800,
              width = 1500,
              scale = 1
            )
          )

        for (col in opts$cols) {
          col_name <- make_clean_names(col, "title")
          col_axis <- filter(col_ranges, name == col)$axis

          add_trace_to_plot <- function(plt, x, y, name) {
            add_trace(
              plt, x = x, y = y,
              name = name, type = "scatter", mode = opts$mode,
              yaxis = col_axis,
              hovertemplate = paste0("%{y:.3~f}", find_unit(col, opts$unit_system)),
              line = list(shape = "spline", width = opts$linewidth)
            )
          }

          if (opts$multi_site) {
            for (id in opts$selected_ids) {
              site_df <- df %>% filter(site_id == id)
              plt <- add_trace_to_plot(
                plt, x = site_df$date, y = site_df[[col]],
                name = sprintf("Site %s: %s", id, col_name)
              )
            }
          } else {
            plt <- add_trace_to_plot(
              plt, x = df$date, y = df[[col]], name = col_name
            )
          }
        }

        plt
      })

      ## download_data // downloadHandler ----
      output$download_data <- downloadHandler(
        filename = function() {
          paste0(download_filename()$csv, ".csv")
        },
        content = function(file) {
          download_data() %>% write_excel_csv(file, na = "")
        }
      )


    } # end module
  )
}
