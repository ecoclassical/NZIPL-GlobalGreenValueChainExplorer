# modules/mod_map.R

mod_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "outer",
        leafletOutput(ns("map"), width = "100%", height = "100%"),
        absolutePanel(
          id = "controls", class = "panel panel-default", fixed = TRUE,
          top = 70, left = 10, width = 360, draggable = TRUE,   # wider here too
          h4("Controls"),
          radioButtons(ns("color_by"), "Legend / color by:",
                       choices = c("Technology" = "technology",
                                   "Type"       = "type",
                                   "Stage"      = "stage"),
                       inline = FALSE),
          sliderInput(ns("year"), "Year", min = 2000, max = 2100,
                      value = 2004, step = 1),  # overwritten in server
          sliderInput(ns("alpha"), "Alpha", min = 0.05, max = 1, value = 0.7, step = 0.05),
          sliderInput(ns("jitter"), "Jitter", min = 0, max = 1, value = 0.1, step = 0.01),
          hr(),
          h5("Top 10 countries"),
          plotOutput(ns("top10"), height = 190),
          h5("Export trend"),
          plotOutput(ns("trend"), height = 190)
        ),
        absolutePanel(
          id = "filters", class = "panel panel-default", fixed = TRUE,
          bottom = 10, left = 380, right = 10, height = 170, draggable = FALSE,
          h4("Filters"),
          fluidRow(
            column(3, selectizeInput(ns("f_tech"), "Technology",
                                     multiple = TRUE, selected = NULL, choices = NULL,
                                     options = list(placeholder = "All technologies"))),
            column(3, selectizeInput(ns("f_type"), "Type",
                                     multiple = TRUE, selected = NULL, choices = NULL,
                                     options = list(placeholder = "All types"))),
            column(3, selectizeInput(ns("f_stage"), "Stage",
                                     multiple = TRUE, selected = NULL, choices = NULL,
                                     options = list(placeholder = "All stages"))),
            column(3, selectizeInput(ns("f_region"), "Region",
                                     multiple = TRUE, selected = NULL, choices = NULL,
                                     options = list(placeholder = "All regions")))
          )
        )
    )
  )
}

mod_map_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # SI number formatter (replaces deprecated label_number_si)
    fmt_num <- scales::label_number(scale_cut = scales::cut_si(" "))
    
    # Populate choices dynamically
    observe({
      updateSelectizeInput(session, "f_tech",  choices = shared$tech_levels)
      updateSelectizeInput(session, "f_type",  choices = shared$type_levels)
      updateSelectizeInput(session, "f_stage", choices = shared$stage_levels)
      updateSelectizeInput(session, "f_region",choices = shared$region_levels)
      
      yrs <- sort(unique(shared$sd$year))
      updateSliderInput(session, "year", min = min(yrs), max = max(yrs),
                        value = min(yrs))
    })
    
    filtered <- reactive({
      df <- shared$sd
      if (length(input$f_tech))   df <- df |> dplyr::filter(technology %in% input$f_tech)
      if (length(input$f_type))   df <- df |> dplyr::filter(type       %in% input$f_type)
      if (length(input$f_stage))  df <- df |> dplyr::filter(stage      %in% input$f_stage)
      if (length(input$f_region)) df <- df |> dplyr::filter(region     %in% input$f_region)
      df |> dplyr::filter(year == input$year)
    })
    
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles("CartoDB.Positron") |>
        leaflet::setView(lng = 10, lat = 20, zoom = 2)
    })
    
    observe({
      df <- filtered()
      pal <- leaflet::colorFactor("Set2", domain = sort(unique(df[[input$color_by]])))
      pts <- df |>
        dplyr::mutate(
          lon  = jitter(lon, amount = input$jitter),
          lat  = jitter(lat, amount = input$jitter),
          fill = pal(.data[[input$color_by]])
        )
      
      leaflet::leafletProxy("map") |>
        leaflet::clearMarkers() |>
        leaflet::addCircleMarkers(
          lng = pts$lon, lat = pts$lat,
          radius = pmax(4, scales::rescale(pts$value, to = c(4, 18))),
          stroke = TRUE, weight = 1,
          color = "#333333",
          fillColor = pts$fill,
          fillOpacity = input$alpha,
          popup = sprintf("<b>%s</b><br/>Year: %s<br/>%s: <b>%s</b><br/>Value: %s",
                          pts$country_name, pts$year,
                          stringr::str_to_title(input$color_by),
                          pts[[input$color_by]], fmt_num(pts$value))
        ) |>
        leaflet::clearControls() |>
        leaflet::addLegend("bottomright", pal = pal, values = pts[[input$color_by]],
                           title = stringr::str_to_title(input$color_by), opacity = 0.9)
    })
    
    # Small plots colored by the same legend choice (stacked / lines by category)
    output$top10 <- renderPlot({
      df <- filtered()
      key <- input$color_by %||% "technology"
      
      df |>
        dplyr::group_by(country_name, .data[[key]]) |>
        dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop_last") |>
        dplyr::group_by(country_name) |>
        dplyr::mutate(total = sum(value)) |>
        dplyr::ungroup() |>
        dplyr::slice_max(order_by = total, n = 10, with_ties = FALSE) |>
        ggplot2::ggplot(ggplot2::aes(x = reorder(country_name, total), y = value, fill = .data[[key]])) +
        ggplot2::geom_col(position = "stack") +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = NULL, fill = stringr::str_to_title(key)) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "none")
    })
    
    output$trend <- renderPlot({
      df <- shared$sd
      # apply same filters (except year) so trend reflects selections
      if (length(input$f_tech))   df <- df |> dplyr::filter(technology %in% input$f_tech)
      if (length(input$f_type))   df <- df |> dplyr::filter(type       %in% input$f_type)
      if (length(input$f_stage))  df <- df |> dplyr::filter(stage      %in% input$f_stage)
      if (length(input$f_region)) df <- df |> dplyr::filter(region     %in% input$f_region)
      
      key <- input$color_by %||% "technology"
      
      df |>
        dplyr::group_by(year, .data[[key]]) |>
        dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
        ggplot2::ggplot(ggplot2::aes(year, value, color = .data[[key]])) +
        ggplot2::geom_line(linewidth = 0.8) +
        ggplot2::labs(x = NULL, y = NULL, color = stringr::str_to_title(key)) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "none")
    })
  })
}