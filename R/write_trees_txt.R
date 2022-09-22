#' Writes trees as .txt files
#' Takes each tree in a phylo or multiPhylo object and writes it as a .txt file
#'
#' @param tree The tree or set of trees to be written.
#' @param path The path to the folder to save the .txt files in.
#' @param tree_names A set tree names. If not given, trees will be numbered.
#'
#' @return A vector containing paths to the new tree .txt files
#'
#' @examples 
#' txt_path <- system.file("txt", package = "groves")
#' path <- paste0(txt_path, "/small_tree_set.txt")
#' tree_set <- ape::read.tree(path)
#' new_tree_set <- standardize_branches(tree_set)
#' write_trees_txt(tree = new_tree_set, path = paste0(txt_path, "/rescaled_small_trees"), 
#'                 tree_names = paste0("tree", 1:3))
#'
#' @export
write_trees_txt <- function(tree, path, tree_names = NULL) {
  # if output folder doesn't exist, make one
  if (!file.exists(path)) {
    dir.create(path)
  }
  # if tree input is a single tree 
  if (inherits(tree, "phylo")) {
    tree_path <- paste0(path, "/tree.txt")
    ape::write.tree(tree, tree_path)
    return(tree_path)
  # if tree input is a set of trees 
  } else if (inherits(tree, "multiPhylo")) {
    # if no names given, number trees
    if (is.null(tree_names)) {
      tree_names <- paste0("tree", 1:length(tree))
    }
    paths <- c()
    for (i in 1:length(tree)) {
      tree_path <- paste0(path, "/", tree_names[i], ".txt")
      ape::write.tree(tree[[i]], tree_path)
      paths <- c(paths, tree_path)
    }
    return(paths)
  } else {
    stop("Please submit a tree (phylo object) or set of trees (multiPhylo object).")
  }
}

