#' Standardizes branch lengths
#' Divides each branch length by the sum of branch lengths or maximum branch length
#' for that tree. 
#'
#' @param tree The tree or set of trees of interest.
#' @param denom The quantity to standardize by. Default is \code{branch_sum}, which is
#' the sum of branch lengths in that tree. The other option is \code{max_branch}, which
#' will divide each branch length by the maximum branch length in that tree. 
#'
#' @return The standardized tree or set of trees. 
#'
#' @examples 
#' path <- system.file("txt", "small_tree_set.txt", package = "groves")
#' tree_set <- ape::read.tree(path)
#' new_tree_set <- standardize_branches(tree_set)
#'
#' @export
standardize_branches <- function(tree, denom = "branch_sum") {
  # check that denom is either "branch_sum" or "max_branch" 
  if (!(denom %in% c("branch_sum", "max_branch"))) {
    stop("Please enter either 'branch_sum' or 'max_branch' for 'denom'.")
  }
  # if tree input is a single tree 
  if (inherits(tree, "phylo")) {
    new_tree <- standardize_tree(tree, denom)
    return(new_tree)
  # if tree input is a set of trees 
  } else if (inherits(tree, "multiPhylo")) {
    # standardize first tree
    new_trees <- standardize_tree(tree[[1]], denom)
    # do the rest
    for (i in 2:length(tree)) {
      new_trees <- c(new_trees, standardize_tree(tree[[i]], denom))
    }
    return(new_trees)
  } else {
    stop("Please submit a tree (phylo object) or set of trees (multiPhylo object).")
  }
}
