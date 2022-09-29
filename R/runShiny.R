#' Run shiny 
#' Runs groves shiny app 
#' 
#' @export
runShiny <- function() {
  appDir <- system.file("shiny-app", package = "groves")
  shiny::runApp(appDir, display.mode = "normal")
}