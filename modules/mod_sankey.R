# modules/mod_sankey.R
# Interactive Sankey for NZIPL Global Green Value Chain Explorer

mod_sankey_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    br(),
    sidebarLayout(
      sidebarPanel(width = 3,
                   uiOutput(ns("tech_ui")),
                   uiOutput(ns("year_ui")),
                   radioButtons(ns("group_by"), "Group by",
                                choices = c("Region", "Country"), inline = TRUE, selected = "Region"),
                   sliderInput(ns("top_n"), "Show top flows (per link set)",
                               min = 10, max = 300, value = 80, step = 10),
                   checkboxInput(ns("normalize"), "Normalize outflows (shares per source)", value = FALSE),
                   helpText("Links are built from bilateral trade of SOURCE stage/type HS6 codes ",
                            "aggregated exporter-group → importer-group, then stitched across stages.")
      ),
      mainPanel(
        networkD3::sankeyNetworkOutput(ns("sankey"), height = "650px"),
        br(),
        DT::DTOutput(ns("links_table"))
      )
    )
  )
}

mod_sankey_server <- function(id, shared){
  moduleServer(id, function(input, output, session){
    
    # ---- guard missing packages nicely ----
    pkgs <- c("networkD3","dplyr","tidyr","stringr","countrycode")
    missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
    if (length(missing)) {
      output$sankey <- renderUI({
        div(style="padding:1rem;",
            strong("Missing packages: "),
            paste(missing, collapse=", "),
            ". Install with: install.packages(c(", 
            paste0(sprintf('\"%s\"', missing), collapse=", "), ")).")
      })
      return(invisible(NULL))
    }
    
    library(dplyr); library(stringr)
    
    # ---------- dictionary (reactive getter; prefers shared) ----------
    get_dict <- reactive({
      d <- isolate(shared$dict)
      if (is.null(d)) {
        p <- file.path("data","green_dictionary.csv")
        if (file.exists(p)) d <- read.csv(p, stringsAsFactors = FALSE)
      }
      d
    })
    
    clean_dict <- reactive({
      d <- get_dict()
      validate(need(!is.null(d), "Dictionary file not found (data/green_dictionary.csv)."))
      d %>%
        mutate(
          HS.Code = sprintf("%06s", gsub("\\D","", as.character(HS.Code))),
          tech    = as.character(ifelse("Technology" %in% names(.), Technology, tech)),
          type    = as.character(ifelse("Type"       %in% names(.), Type,       type)),
          stage   = as.character(ifelse("Stage"      %in% names(.), Stage,      stage))
        ) %>%
        filter(!is.na(HS.Code), !is.na(tech), !is.na(type), !is.na(stage))
    })
    
    output$tech_ui <- renderUI({
      ns <- session$ns
      d <- clean_dict()
      techs <- sort(unique(d$tech))
      selectInput(ns("tech"), "Technology", choices = techs, selected = techs[1])
    })
    
    # ---------- BACI (reactive getter; prefers shared) ----------
    get_baci <- reactive({
      b <- isolate(shared$baci)
      if (is.null(b)) {
        p <- file.path("data","BACI_2002.RDS")
        if (file.exists(p)) b <- readRDS(p)
      }
      b
    })
    
    # Standardize BACI columns to: year, exp_iso3, imp_iso3, hs6, value
    baci_std <- reactive({
      baci <- get_baci()
      validate(need(!is.null(baci), "BACI bilateral trade file not found (data/BACI_2002.RDS)."))
      
      first_match <- function(df, candidates){
        nm <- intersect(candidates, names(df)); if (length(nm)) nm[1] else NA_character_
      }
      cy  <- first_match(baci, c("year","t","Year","anio"))
      ce  <- first_match(baci, c("iso_o","exporter","i","exp_iso3","iso3_o","o","origin"))
      ci  <- first_match(baci, c("iso_d","importer","j","imp_iso3","iso3_d","d","dest"))
      ch  <- first_match(baci, c("hs6","k","product_code","hs","hs_product_code"))
      cv  <- first_match(baci, c("v","value","trade_value_usd","val","export_value","flow","trade_value"))
      
      to_iso3 <- function(x){
        iso <- suppressWarnings(countrycode::countrycode(x, "iso3c", "iso3c"))
        if (mean(!is.na(iso)) < 0.6) {
          iso <- suppressWarnings(countrycode::countrycode(x, "country.name", "iso3c"))
        }
        if (mean(!is.na(iso)) < 0.6 && all(grepl("^\\d+$", x))) {
          iso <- suppressWarnings(countrycode::countrycode(as.integer(x), "un", "iso3c"))
        }
        iso[is.na(iso) & nchar(x) == 3] <- x[is.na(iso) & nchar(x) == 3]
        iso
      }
      
      baci %>%
        mutate(
          year  = if (!is.na(cy))  .data[[cy]]  else 2002,
          exp   = if (!is.na(ce))  .data[[ce]]  else NA,
          imp   = if (!is.na(ci))  .data[[ci]]  else NA,
          hs6   = if (!is.na(ch))  .data[[ch]]  else NA,
          value = if (!is.na(cv))  .data[[cv]]  else NA
        ) %>%
        mutate(
          hs6   = sprintf("%06s", gsub("\\D","", as.character(hs6))),
          value = as.numeric(value),
          exp   = as.character(exp),
          imp   = as.character(imp)
        ) %>%
        filter(!is.na(exp), !is.na(imp), !is.na(hs6), !is.na(value), is.finite(value), value > 0) %>%
        mutate(
          exp_iso3 = {
            x <- to_iso3(exp)
            ifelse(is.na(x) & nchar(exp) == 3, exp, x)
          },
          imp_iso3 = {
            x <- to_iso3(imp)
            ifelse(is.na(x) & nchar(imp) == 3, imp, x)
          }
        ) %>%
        filter(!is.na(exp_iso3), !is.na(imp_iso3))
    })
    
    years <- reactive(sort(unique(baci_std()$year)))
    output$year_ui <- renderUI({
      ns <- session$ns
      selectInput(ns("year"), "Year", choices = years(), selected = tail(years(), 1))
    })
    
    # ---- Pipeline spec (ordered stage-to-stage links) ----
    pipe_spec <- list(
      list(from = list(stage="Upstream",   type="Raw Material"),
           to   = list(stage="Midstream",  type="Processed Material")),
      list(from = list(stage="Midstream",  type="Processed Material"),
           to   = list(stage="Downstream", type="Product Component")),
      list(from = list(stage="Downstream", type="Product Component"),
           to   = list(stage="Downstream", type="Process Equipment"))
    )
    
    # Palette (reuse shared$pal_type if available)
    get_palette <- reactive({
      pal_shared <- isolate(shared$pal_type)
      if (!is.null(pal_shared)) return(pal_shared)
      d <- clean_dict()
      tlev <- sort(unique(d$type))
      cols <- RColorBrewer::brewer.pal(max(3, min(8, length(tlev))), "Set2")
      leaflet::colorFactor(cols, domain = tlev)
    })
    
    # Build sankey (reactive) ---------------------------------------
    build_sankey <- reactive({
      req(input$tech, input$year, input$group_by)
      
      pal <- get_palette()
      dict_tech <- clean_dict() %>% filter(tech == input$tech)
      baci_df <- baci_std()
      
      group_lab <- function(iso3){
        if (identical(input$group_by, "Region")) {
          out <- suppressWarnings(countrycode::countrycode(iso3, "iso3c", "un.region.name"))
          out[is.na(out)] <- "Other/Unknown"; out
        } else {
          out <- suppressWarnings(countrycode::countrycode(iso3, "iso3c", "country.name"))
          out[is.na(out)] <- iso3; out
        }
      }
      
      links_all <- list()
      node_rows <- tibble::tibble(name=character(), type=character(), stage=character())
      
      for (k in seq_along(pipe_spec)) {
        spec <- pipe_spec[[k]]
        s_stage <- spec$from$stage; s_type <- spec$from$type
        t_stage <- spec$to$stage;   t_type <- spec$to$type
        
        hs_src <- dict_tech %>% filter(stage==s_stage, type==s_type) %>% pull(HS.Code) %>% unique()
        if (!length(hs_src)) next
        
        flows <- baci_df %>%
          filter(year == input$year, hs6 %in% hs_src) %>%
          mutate(src_group = group_lab(exp_iso3),
                 dst_group = group_lab(imp_iso3)) %>%
          group_by(src_group, dst_group) %>%
          summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
          arrange(desc(value)) %>%
          slice_head(n = input$top_n)
        
        if (isTRUE(input$normalize) && nrow(flows)) {
          flows <- flows %>%
            group_by(src_group) %>%
            mutate(value = ifelse(sum(value) > 0, value / sum(value), 0)) %>%
            ungroup()
        }
        
        src_label <- paste0(s_stage, ": ", s_type, " [", flows$src_group, "]")
        dst_label <- paste0(t_stage, ": ", t_type, " [", flows$dst_group, "]")
        
        node_rows <- bind_rows(
          node_rows,
          tibble::tibble(name = unique(src_label), type = s_type, stage = s_stage),
          tibble::tibble(name = unique(dst_label), type = t_type, stage = t_stage)
        )
        
        links_all[[k]] <- flows %>%
          transmute(
            source_lab = src_label,
            target_lab = dst_label,
            value
          )
      }
      
      links_df <- dplyr::bind_rows(links_all)
      validate(need(nrow(links_df) > 0, "No flows for this tech/year under current filters."))
      
      nodes <- node_rows %>% distinct(name, .keep_all = TRUE) %>% mutate(id = row_number() - 1L)
      
      links <- links_df %>%
        left_join(nodes %>% select(name, id), by = c("source_lab"="name")) %>%
        rename(source = id) %>%
        left_join(nodes %>% select(name, id), by = c("target_lab"="name")) %>%
        rename(target = id) %>%
        mutate(group = stringr::str_extract(source_lab, "(?<=: ).*?(?= \\[)")) %>% # type of source
        select(source, target, value, group)
      
      type_levels <- unique(nodes$type)
      cols <- pal(type_levels)
      colourScale <- sprintf(
        "d3.scaleOrdinal().domain(%s).range(%s)",
        jsonlite::toJSON(unname(type_levels)),
        jsonlite::toJSON(unname(cols))
      )
      
      list(nodes = nodes, links = links, colourScale = colourScale)
    })
    
    # Render Sankey
    output$sankey <- networkD3::renderSankeyNetwork({
      sk <- build_sankey()
      networkD3::sankeyNetwork(
        Links = sk$links, Nodes = sk$nodes,
        Source = "source", Target = "target", Value = "value",
        NodeID = "name",
        NodeGroup = "type", LinkGroup = "group",
        sinksRight = TRUE, fontSize = 12, nodeWidth = 24, iterations = 0,
        colourScale = sk$colourScale
      )
    })
    
    # QA table
    output$links_table <- DT::renderDT({
      sk <- build_sankey()
      L <- sk$links %>%
        left_join(sk$nodes %>% select(id, name), by = c("source"="id")) %>%
        rename(source_name = name) %>%
        left_join(sk$nodes %>% select(id, name), by = c("target"="id")) %>%
        rename(target_name = name) %>%
        mutate(value = if (isTRUE(input$normalize)) round(100*value, 1) else round(value, 0)) %>%
        arrange(desc(value))
      DT::datatable(L, options = list(pageLength = 15, scrollX = TRUE))
    })
    
  })
}