#' Recodes outgroup edge.
#' Recodes the branch going to an outgroup to the value provided.
#'
#' @param tree The tree of interest.
#' @param outgroup The name of the outgroup in the tree of interest.
#' @param value The value to make the outgroup edge. If null, defaults to the second largest
#' length on the tree of interest.
#'
#' @return The tree of interest with the outgroup edge changed to the value provided.
#'
#'
#' @export
recode_outgroup <- function(tree, outgroup, value = NULL) {
  if (is.null(value)) {
    value <- sort(tree$edge.length, decreasing = TRUE)[2]
  }
  outgroup_ind <- which(tree$tip.label == outgroup)
  if (length(outgroup_ind) == 0) {
    stop("Please provide outgroup included on tree of interest.")
  }
  edge_ind <- which(tree$edge[,2] == outgroup_ind)
  new_tree <- tree
  new_tree$edge.length[edge_ind] <- value
  return(new_tree)
}
