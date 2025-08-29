# modules/mod_regionplots.R

mod_regionplots_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        width = 3,
        div(class = "panel panel-default", style = "padding:12px;",
            h4("Controls"),
            radioButtons(ns("color_by"), "Legend / color by:",
                         choices = c("Technology" = "technology",
                                     "Type"       = "type",
                                     "Stage"      = "stage"),
                         inline = FALSE),
            hr(),
            h4("Filters"),
            selectizeInput(ns("f_tech"),  "Technology", multiple = TRUE, choices = NULL),
            selectizeInput(ns("f_type"),  "Type",       multiple = TRUE, choices = NULL),
            selectizeInput(ns("f_stage"), "Stage",      multiple = TRUE, choices = NULL),
            selectizeInput(ns("f_region"),"Region",     multiple = TRUE, choices = NULL)
        )
      ),
      column(
        width = 9,
        plotOutput(ns("region_plot"), height = 650)
      )
    )
  )
}

mod_regionplots_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      updateSelectizeInput(session, "f_tech",  choices = shared$tech_levels)
      updateSelectizeInput(session, "f_type",  choices = shared$type_levels)
      updateSelectizeInput(session, "f_stage", choices = shared$stage_levels)
      updateSelectizeInput(session, "f_region",choices = shared$region_levels)
      if (is.null(input$color_by)) updateRadioButtons(session, "color_by", selected = "technology")
    })
    
    filtered <- reactive({
      df <- shared$sd
      if (length(input$f_tech))   df <- df |> dplyr::filter(technology %in% input$f_tech)
      if (length(input$f_type))   df <- df |> dplyr::filter(type       %in% input$f_type)
      if (length(input$f_stage))  df <- df |> dplyr::filter(stage      %in% input$f_stage)
      if (length(input$f_region)) df <- df |> dplyr::filter(region     %in% input$f_region)
      df
    })
    
    output$region_plot <- renderPlot({
      key <- input$color_by %||% "technology"
      filtered() |>
        dplyr::group_by(region, .data[[key]]) |>
        dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop_last") |>
        dplyr::group_by(region) |>
        dplyr::mutate(total = sum(value)) |>
        dplyr::ungroup() |>
        ggplot2::ggplot(ggplot2::aes(x = reorder(region, total), y = value, fill = .data[[key]])) +
        ggplot2::geom_col(position = "stack") +
        ggplot2::coord_flip() +
        ggplot2::labs(title = "Exports by region", x = NULL, y = "Exports", fill = stringr::str_to_title(key)) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
    })
  })
}