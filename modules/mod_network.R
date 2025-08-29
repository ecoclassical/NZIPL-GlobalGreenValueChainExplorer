# modules/mod_network.R

mod_network_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        width = 3,
        div(class = "panel panel-default", style = "padding:12px;",
            h4("Controls"),
            radioButtons(ns("color_nodes"), "Node color",
                         choices = c("Region" = "region"), selected = "region"),
            helpText("Edges = exporter → importer, sum(value) for BACI 2002 only."),
            hr(),
            h4("Filters"),
            selectizeInput(ns("f_tech"),  "Technology",       multiple = TRUE, choices = NULL),
            selectizeInput(ns("f_type"),  "Type",             multiple = TRUE, choices = NULL),
            selectizeInput(ns("f_stage"), "Stage",            multiple = TRUE, choices = NULL),
            selectizeInput(ns("f_region"),"Region (nodes)",   multiple = TRUE, choices = NULL)
        )
      ),
      column(
        width = 9,
        visNetwork::visNetworkOutput(ns("net"), height = "720px")
      )
    )
  )
}

mod_network_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    
    fmt_num <- scales::label_number(scale_cut = scales::cut_si(" "))
    
    observe({
      updateSelectizeInput(session, "f_tech",   choices = shared$tech_levels)
      updateSelectizeInput(session, "f_type",   choices = shared$type_levels)
      updateSelectizeInput(session, "f_stage",  choices = shared$stage_levels)
      updateSelectizeInput(session, "f_region", choices = shared$region_levels)
    })
    
    edges_nodes <- reactive({
      df <- shared$baci2002
      if (length(input$f_tech))   df <- dplyr::filter(df, technology %in% input$f_tech)
      if (length(input$f_type))   df <- dplyr::filter(df, type       %in% input$f_type)
      if (length(input$f_stage))  df <- dplyr::filter(df, stage      %in% input$f_stage)
      
      edges <- df |>
        dplyr::group_by(exporter, importer) |>
        dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
        dplyr::filter(value > 0)
      
      node_codes <- union(edges$exporter, edges$importer)
      
      country_meta_iso3 <- shared$country_meta |>
        dplyr::transmute(
          country_iso3 = toupper(country_iso3),
          label_name   = dplyr::coalesce(country_name, country_iso3),
          region
        ) |>
        dplyr::distinct(country_iso3, .keep_all = TRUE)
      
      nodes <- tibble::tibble(id = node_codes) |>
        dplyr::left_join(country_meta_iso3, by = c("id" = "country_iso3")) |>
        dplyr::mutate(
          label = dplyr::coalesce(label_name, id),
          group = dplyr::coalesce(region, "Other")
        ) |>
        dplyr::select(id, label, group)
      
      if (length(input$f_region)) {
        nodes <- dplyr::filter(nodes, group %in% input$f_region)
      }
      
      edges <- edges |>
        dplyr::filter(exporter %in% nodes$id, importer %in% nodes$id) |>
        dplyr::transmute(from = exporter, to = importer, value,
                         title = fmt_num(value))
      
      list(edges = edges, nodes = nodes)
    })
    
    output$net <- visNetwork::renderVisNetwork({
      en <- edges_nodes()
      visNetwork::visNetwork(
        nodes = en$nodes, edges = en$edges
      ) |>
        visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) |>
        visNetwork::visLegend() |>
        visNetwork::visPhysics(stabilization = TRUE) |>
        visNetwork::visIgraphLayout(layout = "layout_with_fr")
    })
  })
}