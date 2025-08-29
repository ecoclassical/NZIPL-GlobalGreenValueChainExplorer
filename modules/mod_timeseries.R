# modules/mod_timeseries.R
# Interactive time series: Plotly if available, otherwise ggplot fallback.

mod_timeseries_ui <- function(id){
  ns <- NS(id)
  
  use_plotly <- requireNamespace("plotly", quietly = TRUE)
  
  sidebar <- sidebarPanel(
    width = 3,
    selectInput(ns("agg_by"), "Aggregate by",
                choices = c("Country", "Region"), selected = "Region"),
    selectInput(ns("color_by"), "Color by",
                choices = c("Type", "Stage"), selected = "Type"),
    sliderInput(ns("year_range"), "Years", min = 1995, max = 2025,
                value = c(2010, 2025), step = 1, sep = "")
  )
  
  main <- mainPanel(
    if (use_plotly) {
      plotly::plotlyOutput(ns("ts_plot"), height = 520)
    } else {
      plotOutput(ns("ts_plot_static"), height = 520)
    },
    br(),
    DT::DTOutput(ns("ts_table"))
  )
  
  fluidPage(br(), sidebarLayout(sidebar, main))
}

mod_timeseries_server <- function(id, shared){
  moduleServer(id, function(input, output, session){
    
    library(dplyr)
    library(ggplot2)
    
    use_plotly <- requireNamespace("plotly", quietly = TRUE)
    
    # Data prep
    ts_data <- reactive({
      req(shared$series)
      df <- shared$series %>%
        filter(between(year, input$year_range[1], input$year_range[2]))
      
      if (input$agg_by == "Region") {
        df <- df %>%
          group_by(year, region, type, stage) %>%
          summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
      } else {
        df <- df %>%
          group_by(year, country, type, stage) %>%
          summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
      }
      df
    })
    
    # Plotly version (if available)
    if (use_plotly) {
      output$ts_plot <- plotly::renderPlotly({
        df <- ts_data()
        req(nrow(df) > 0)
        
        color_var <- if (identical(input$color_by, "Type")) "type" else "stage"
        group_var <- if (identical(input$agg_by,  "Region")) "region" else "country"
        
        p <- ggplot(df, aes(x = year, y = value,
                            color = .data[[color_var]],
                            group = interaction(.data[[group_var]], .data[[color_var]]))) +
          geom_line() +
          labs(title = "Time Series", x = NULL, y = NULL, color = NULL) +
          theme_minimal(base_size = 12) +
          theme(plot.title = element_text(face = "bold"))
        
        plotly::ggplotly(p, dynamicTicks = TRUE)
      })
    } else {
      # Static fallback
      output$ts_plot_static <- renderPlot({
        df <- ts_data()
        req(nrow(df) > 0)
        
        color_var <- if (identical(input$color_by, "Type")) "type" else "stage"
        group_var <- if (identical(input$agg_by,  "Region")) "region" else "country"
        
        ggplot(df, aes(x = year, y = value,
                       color = .data[[color_var]],
                       group = interaction(.data[[group_var]], .data[[color_var]]))) +
          geom_line() +
          labs(title = "Time Series", x = NULL, y = NULL, color = NULL) +
          theme_minimal(base_size = 12) +
          theme(plot.title = element_text(face = "bold"))
      })
    }
    
    # Table
    output$ts_table <- DT::renderDT({
      df <- ts_data()
      DT::datatable(df, options = list(pageLength = 15, scrollX = TRUE))
    })
  })
}