#' Runs shiny app
#' Runs shiny app of groves with user supplied data or with an example Prevotella dataset.
#'
#' @return Nothing is returned.
#' 
#' @export
run_shiny <- function() {
  appDir <- system.file("shiny-app", package = "groves")
  shiny::runApp(appDir, display.mode = "normal")
}

