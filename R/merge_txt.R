#' Merge txt files
#' Takes the contents of separate txt files and inputs each file as a line in a single file.
#'
#' @param paths A set of paths to txt files, each containing a single line. 
#' @param output_path A path to save the output txt file as. 
#' 
#' @examples 
#' path <- paste0(system.file("txt", package = "groves"), "/")
#' tree_paths <- paste0(path, "tree", 1:3, ".txt")
#' merge_txt(paths = tree_paths, output_path = paste0(path, "test_trees.txt"))
#'
#' @export
merge_txt <- function(paths, output_path) {
  all_trees <- lapply(paths, ape::read.tree)
  class(all_trees) <- "multiPhylo"
  ape::write.tree(phy = all_trees, file = output_path)
}
