#' Runs shiny app
#' Runs shiny app of groves with user supplied data or with an example Prevotella dataset.
#'
#' @return Nothing is returned.
#' 
#' @export
run_shiny <- function() {
  appDir <- "inst/shiny-app/"
  shiny::runApp(appDir, display.mode = "normal")
}

