# modules/mod_radar.R

mod_radar_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    card(
      class = "chart-card",
      card_header("Radar: Technology shares by Type"),
      plotOutput(ns("radar"), height = 520)
    )
  )
}

mod_radar_server <- function(id, shared){
  moduleServer(id, function(input, output, session){
    
    output$radar <- renderPlot({
      # Example: aggregate values by Type for a few Technologies (top 5 by total)
      agg <- shared$sd %>%
        group_by(Technology, Type) %>%
        summarise(v = sum(value, na.rm = TRUE), .groups = "drop")
      
      tech_top <- agg %>% group_by(Technology) %>%
        summarise(t = sum(v), .groups = "drop") %>%
        arrange(desc(t)) %>% slice_head(n = 5) %>% pull(Technology)
      
      mat <- agg %>%
        filter(Technology %in% tech_top) %>%
        tidyr::pivot_wider(names_from = Type, values_from = v, values_fill = 0)
      
      # fmsb expects first 2 rows = max/min
      vals <- mat %>% select(-Technology)
      maxv <- apply(vals, 2, max); minv <- rep(0, ncol(vals))
      radar_df <- rbind(maxv, minv, vals)
      rownames(radar_df) <- c("max","min", mat$Technology)
      
      radarchart(radar_df, axistype = 1, seg = 5, pcol = 1:nrow(vals),
                 plwd = 2, cglcol = "grey80", cglty = 1, axislabcol = "grey40",
                 caxislabels = pretty(range(maxv), 5), vlcex = 0.8)
      legend("bottom", legend = mat$Technology, col = 1:nrow(vals), lwd = 2, bty = "n", ncol = 2)
    })
  })
}