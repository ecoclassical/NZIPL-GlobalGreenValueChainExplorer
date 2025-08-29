# modules/mod_map.R

mod_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "outer",
        leafletOutput(ns("map"), width = "100%", height = "100%"),
        absolutePanel(
          id = "controls", class = "panel panel-default", fixed = TRUE,
          top = 70, left = 10, width = 300, draggable = TRUE,
          h4("Controls"),
          # inline = TRUE makes them horizontal; CSS will enforce 3 columns
          radioButtons(ns("color_by"), "Legend / color by:",
                       choices = c("Tech" = "technology",
                                   "Type"       = "type",
                                   "Stage"      = "stage"),
                       inline = TRUE),
          # sep="" removes thousands separators on the slider labels
          sliderInput(ns("year"), "Year", min = 2000, max = 2100,
                      value = 2004, step = 1, sep = ""),
          sliderInput(ns("alpha"), "Alpha",  min = 0.05, max = 1, value = 0.7, step = 0.05),
          sliderInput(ns("jitter"), "Jitter", min = 0,    max = 1, value = 0.1, step = 0.01),
          hr(),
          h5("Top 10 countries"),
          plotOutput(ns("top10"), height = 125),   # smaller
          h5("Export trend"),
          plotOutput(ns("trend"), height = 125)    # smaller
        ),
        absolutePanel(
          id = "filters", class = "panel panel-default", fixed = TRUE,
          bottom = 10, left = 380, right = 10, height = 170, draggable = FALSE,
          h4("Filters"),
          fluidRow(
            column(3, selectizeInput(ns("f_tech"),  "Technology",
                                     multiple = TRUE, selected = NULL, choices = NULL,
                                     options = list(placeholder = "All technologies"))),
            column(3, selectizeInput(ns("f_type"),  "Type",
                                     multiple = TRUE, selected = NULL, choices = NULL,
                                     options = list(placeholder = "All types"))),
            column(3, selectizeInput(ns("f_stage"), "Stage",
                                     multiple = TRUE, selected = NULL, choices = NULL,
                                     options = list(placeholder = "All stages"))),
            column(3, selectizeInput(ns("f_region"),"Region",
                                     multiple = TRUE, selected = NULL, choices = NULL,
                                     options = list(placeholder = "All regions")))
          )
        )
    )
  )
}

mod_map_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    
    # SI number formatter
    fmt_num <- scales::label_number(scale_cut = scales::cut_si(" "))
    
    # Populate choices dynamically
    observe({
      updateSelectizeInput(session, "f_tech",  choices = shared$tech_levels)
      updateSelectizeInput(session, "f_type",  choices = shared$type_levels)
      updateSelectizeInput(session, "f_stage", choices = shared$stage_levels)
      updateSelectizeInput(session, "f_region",choices = shared$region_levels)
      
      # update year slider only if data is nonempty
      yrs <- sort(unique(shared$sd$year))
      if (length(yrs) > 0) {
        updateSliderInput(session, "year",
                          min = min(yrs), max = max(yrs),
                          value = min(yrs), step = 1)
      }
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
      leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = FALSE)) |>
        leaflet::addProviderTiles("CartoDB.Positron") |>
        leaflet::setView(lng = 10, lat = 20, zoom = 2) |>
        htmlwidgets::onRender("
      function(el, x) {
        var map = this;
        L.control.zoom({ position: 'topright' }).addTo(map);
      }
    ")
    })
    
    observe({
      df  <- filtered()
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
        leaflet::addLegend("topright", pal = pal, values = pts[[input$color_by]],
                           title = stringr::str_to_title(input$color_by), opacity = 0.9)
    })
    
    # Small plots colored by the same legend choice
    output$top10 <- renderPlot({
      df  <- filtered()
      key <- input$color_by %||% "technology"
      
      # use the same domain (order) as the map legend
      domain_levels <- switch(key,
                              technology = shared$tech_levels,
                              type       = shared$type_levels,
                              stage      = shared$stage_levels)
      
      df |>
        dplyr::group_by(country_name, .data[[key]]) |>
        dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop_last") |>
        dplyr::group_by(country_name) |>
        dplyr::mutate(total = sum(value)) |>
        dplyr::ungroup() |>
        dplyr::slice_max(order_by = total, n = 10, with_ties = FALSE) |>
        dplyr::mutate(.grp = factor(.data[[key]], levels = domain_levels)) |>
        ggplot2::ggplot(ggplot2::aes(reorder(country_name, total), value, fill = .grp)) +
        ggplot2::geom_col(position = "stack") +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = NULL, fill = stringr::str_to_title(key)) +
        ggplot2::scale_fill_brewer(palette = "Set2", drop = FALSE) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(legend.position = "none",
                       axis.title = element_blank())
    })
    
    output$trend <- renderPlot({
      df <- shared$sd
      if (length(input$f_tech))   df <- df |> dplyr::filter(technology %in% input$f_tech)
      if (length(input$f_type))   df <- df |> dplyr::filter(type       %in% input$f_type)
      if (length(input$f_stage))  df <- df |> dplyr::filter(stage      %in% input$f_stage)
      if (length(input$f_region)) df <- df |> dplyr::filter(region     %in% input$f_region)
      
      key <- input$color_by %||% "technology"
      domain_levels <- switch(key,
                              technology = shared$tech_levels,
                              type       = shared$type_levels,
                              stage      = shared$stage_levels)
      
      df |>
        dplyr::group_by(year, .data[[key]]) |>
        dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(.grp = factor(.data[[key]], levels = domain_levels)) |>
        ggplot2::ggplot(ggplot2::aes(year, value, color = .grp)) +
        ggplot2::geom_line(linewidth = 0.8) +
        ggplot2::labs(x = NULL, y = NULL, color = stringr::str_to_title(key)) +
        ggplot2::scale_color_brewer(palette = "Set2", drop = FALSE) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(legend.position = "none",
                       axis.title = element_blank())
    })
  })
}