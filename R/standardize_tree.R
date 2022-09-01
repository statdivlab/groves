#' Standardizes branch lengths for a single tree
#' For a single tree, divides each branch length by the sum of branch lengths or 
#' maximum branch length for that tree or sets all branch lengths to \code{1}. 
#'
#' @param tree The tree of interest.
#' @param denom The quantity to standardize by. Default is \code{branch_sum}, which is
#' the sum of branch lengths in that tree. Another option is \code{max_branch}, which
#' will divide each branch length by the maximum branch length in that tree. The last 
#' option is \code{one}, which recodes all branches to have length \code{1}.
#'
#' @return The standardized tree. 
#'
#' @examples 
#' path <- system.file("txt", "small_tree_set.txt", package = "groves")
#' tree_set <- ape::read.tree(path)
#' new_tree <- standardize_tree(tree_set[[1]])
#'
#' @export
standardize_tree <- function(tree, denom = "branch_sum") {
  # standardize branches 
  new_tree <- tree
  # check that tree doesn't have all branch lengths equal to 0 
  if (sum(tree$edge.length) == 0) {
    stop("All branch lengths in tree are equal to 0. This tree cannot be standardized.")
  }
  # standardize by sum of branch lengths
  if (denom == "branch_sum") {
    new_tree$edge.length <- new_tree$edge.length/(sum(new_tree$edge.length))
  # standardize by maximum branch length 
  } else if (denom == "max_branch") {
    new_tree$edge.length <- new_tree$edge.length/(max(new_tree$edge.length))
  # set each branch length to 1
  } else {
    new_tree$edge.length <- rep(1, length(new_tree$edge.length))
  }
  return(new_tree)
}
