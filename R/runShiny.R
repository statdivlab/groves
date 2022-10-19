#' Run shiny 
#' Runs groves shiny app 
#' 
#' @export
runShiny <- function() {
  library(groves)
  library(plotly)
  library(ggtree)
  # build with prevotella to start
  path <- system.file("prevotella", package = "groves")
  gene_names <- stringr::str_sub(list.files(paste0(path, "/gene_trees")), 1, -5)
  # make a vector of paths to .txt files of all trees to go into the plot 
  tree_paths <- c(paste0(path, "/gene_trees/", gene_names, ".txt"),
                  paste0(path, "/phylogenomic_trees/concat_tree.txt"))
  phylogenomic <- length(tree_paths)
  
  appDir <- "inst/shiny-app/"
  #appDir <- system.file("shiny-app", package = "groves")
  shiny::runApp(appDir, display.mode = "normal")
}
