# modules/mod_dictionary.R
mod_dictionary_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h3("Green Dictionary"),
    DTOutput(ns("dict"))
  )
}

mod_dictionary_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    output$dict <- renderDT({
      datatable(
        shared$dictionary,
        rownames = FALSE,
        options = list(pageLength = 25, autoWidth = TRUE)
      )
    })
  })
}