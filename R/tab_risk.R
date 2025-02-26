
riskUI <- function() {
  ns <- NS("risk")
  div(
    style = "margin-top: 10px;",
    # p(em("Select a crop of interest below to see site-specific disease risk predictions.")),
    uiOutput(ns("crop_ui")),
    uiOutput(ns("crop_info_ui")),
    uiOutput(ns("main_ui"))
  )
}

riskServer <- function(wx_data, selected_site, sites_ready, weather_ready) {
  moduleServer(
    id = "risk",
    function(input, output, session) {
      ns <- session$ns

      output$crop_ui <- renderUI({
        crop_choices <- OPTS$risk_crop_choices

        tagList(
          radioGroupButtons(
            inputId = ns("crop"),
            label = "Crop type",
            choices = crop_choices,
            selected = first_truthy(input$crops, first(crop_choices)),
            individual = TRUE,
            size = "sm"
          )
        )
      })

      output$crop_info_ui <- renderUI({
        crop <- req(input$crop)
        div(
          style = "margin: 10px 0; font-style: italic;",
          OPTS$risk_info[[crop]]
        )
      })

      output$main_ui <- renderUI({
        validate(need(sites_ready(), OPTS$validation_sites_ready))
        validate(need(weather_ready(), OPTS$validation_weather_ready))

        tagList(
          uiOutput(ns("opts_ui")),
          uiOutput(ns("selected_site_ui")),
          uiOutput(ns("plots_ui"))
        )
      })

      output$opts_ui <- renderUI({
        crop <- req(input$crop)
        irrig_choices <- list("Dry" = "dry", "Irrigated" = "irrig")
        if (crop == "corn") {
          spacing_label <- "Row spacing:"
          spacing_choices <- list("30-inch" = "30", "15-inch" = "15")
        } else {
          spacing_label <- "Canopy closure:"
          spacing_choices <- list("Open" = "30", "Closed" = "15")
        }

        div(
          class = "inline-flex",
          style = "margin: 0 10px;",
          radioButtons(
            inputId = ns("irrigation"),
            label = "Irrigation:",
            choices = irrig_choices,
            selected = input$irrigation %||% "irrig",
            inline = TRUE
          ),
          conditionalPanel(
            "input['risk-irrigation'] == 'irrig'",
            radioButtons(
              inputId = ns("spacing"),
              label = spacing_label,
              choices = spacing_choices,
              selected = input$spacing %||% "30",
              inline = TRUE
            )
          ),
          uiOutput(ns("show_all_sites"))
        )
      })

      # output$selected_site_ui <- renderUI({
      #   sites <- wx_data()$sites
      #   req(nrow(sites) > 1)
      #   materialSwitch(
      #     inputId = ns("selected_site_only"),
      #     label = "Only show selected site",
      #     value = input$selected_site_only %||% FALSE
      #   )
      # })

      output$show_all_sites <- renderUI({
        sites <- wx_data()$sites
        req(nrow(sites) > 1)
        radioButtons(
          inputId = ns("show_all_sites"),
          label = "Show results for:",
          choices = list(
            "All sites" = TRUE,
            "Selected site" = FALSE
          ),
          selected = input$show_all_sites %||% TRUE,
          inline = TRUE
        )
      })

      selected_models <- reactive({
        crop <- req(input$crop)
        c(
          "White mold" = switch(req(input$irrigation),
            "dry" = "white_mold_dry_prob",
            "irrig" = switch(req(input$spacing),
              "30" = "white_mold_irrig_30_prob",
              "15" = "white_mold_irrig_15_prob"
            )
          ),
          if (crop == "corn") c(
            "Gray leaf spot" = "gray_leaf_spot_prob",
            "Tarspot" = "tarspot_prob"
          ),
          if (crop == "soy") c(
            "Frogeye leaf spot" = "frogeye_leaf_spot_prob"
          ),
          if (crop == "potato") c(
            "Early blight (P-days)" = "potato_pdays",
            "Late blight (DSV)" = "late_blight_dsv"
          ),
          if (crop == "carrot") c(
            "Alternaria (DSV)" = "alternaria_dsv"
          ),
          if (crop == "beet") c(
            "Cercospora (DIV)" = "cercospora_div"
          )
        )
      })

      output$plots_ui <- renderUI({
        models <- selected_models()
        wx <- wx_data()
        dates <- wx$dates
        sites <- wx$sites %>%
          mutate(site_label = sprintf("Site %s: %s", id, name))
        if (nrow(sites) > 1) {
          if (req(!is.null(input$show_all_sites)) & input$show_all_sites == FALSE) {
            sites <- sites %>% filter(id == selected_site())
          }
        }
        disease_data <- wx$disease %>%
          select(grid_id, date, all_of(models)) %>%
          pivot_longer(cols = -c(grid_id, date)) %>%
          left_join(
            enframe(models, value = "model"),
            join_by(name)
          )
        site_data <- sites %>%
          st_drop_geometry() %>%
          select(site_label, grid_id) %>%
          left_join(disease_data, join_by(grid_id)) %>%
          mutate(name = fct_inorder(name))
        site_labels <- unique(sites$site_label)


        elems <- lapply(site_labels, function(label) {
          df <- site_data %>%
            filter(site_label == !!label, !is.na(grid_id))

          content <- if (nrow(df) > 0) {
            df <- df %>%
              mutate(assign_risk(first(model), value), .by = model)
            last_value <- df %>%
              filter(date == max(date)) %>%
              mutate(risk_label = sprintf("%s: %s (%.0f%%)", name, risk, value * 100))
            risk_date <- first(last_value$date)
            risk_info <- paste(last_value$risk_label, collapse = ", ")
            plt <- disease_plot(df, xrange = c(dates$start, dates$end))

            tagList(
              plt,
              div(
                style = "margin-top: 10px; font-style: italic;",
                strong(paste0("For ", format(risk_date, "%b %d, %Y"), ":")),
                risk_info
              )
            )
          } else {
            div(
              span(style = "color: orange;", icon("warning")),
              em("This site does not have any weather data downloaded yet. Press 'Fetch weather' on the sidebar to download any missing data.")
            )
          }

          div(
            style = "border: 1px solid hsl(210, 40%, 80%); border-radius: 5px;",
            div(
              style = "background: hsl(210, 40%, 95%); padding: 5px 10px; font-size: large; font-weight: bold; border-radius: 5px;",
              label,
            ),
            div(
              style = "background: white; padding: 5px 10px; border-radius: 5px;",
              content,
            )
          )
        })

        div(
          style = "display: flex; flex-direction: column; gap: 10px; max-height: 100vh; overflow: auto;",
          elems
        )
      })

    } # end module
  )
}
