#' Check if binary 
#' Given a tree or set of trees, checks if it is binary, i.e. for a rooted tree with \code{n} tips it has \code{n - 1} internal nodes, 
#' for a unrooted tree with \code{n} tips it has \code{n - 2} internal nodes, and all internal edges have length 
#' greater than \code{0}.
#'
#' @param tree OPTIONAL, the tree(s) to check. Must be included if \code{tree_path} is not. Can be a phylo or 
#' multiPhylo object. 
#' @param tree_path OPTIONAL, the path to the tree(s) to check. Must be included if \code{tree} is not.
#' 
#' @return A boolean (or list of booleans), TRUE if the tree is binary. 
#' 
#' @examples 
#' path <- system.file("txt/tree1.txt", package = "groves")
#' tree <- ape::read.tree(path)
#' check_binary(tree)
#'
#' @export
check_binary <- function(tree = NULL, tree_path = NULL) {
  if (is.null(tree_path) & is.null(tree)) {
    stop("Please input either a tree or path to a tree.")
  }
  if (is.null(tree)) {
    tree <- ape::read.tree(tree_path)
  }
  if (inherits(tree, "phylo")) {
    binary <- check_binary_single(tree)
  } else if (inherits(tree, "multiPhylo")) {
    n_tree <- length(tree)
    binary <- rep(NA, n_tree) 
    for (i in 1:n_tree) {
      binary[i] <- check_binary_single(tree[[i]])
    }
  } else {
    stop("tree must be a phylo or multiPhylo object.")
  }
  return(binary)
}
