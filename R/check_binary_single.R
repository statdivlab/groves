#' Check if single tree is binary 
#' Given a single tree, checks if it is binary, i.e. for a rooted tree with \code{n} tips it has \code{n - 1} internal nodes, 
#' for a unrooted tree with \code{n} tips it has \code{n - 2} internal nodes, and all internal edges have length 
#' greater than \code{0}.
#'
#' @param tree the tree to check. 
#' 
#' @return A boolean, TRUE if the tree is binary. 
check_binary_single <- function(tree) {
  # check that there are the right number of nodes for a binary tree
  binary_number_nodes <- (tree$Nnode + !ape::is.rooted(tree)) == 
    (length(tree$tip.label) - 1)
  if (binary_number_nodes) {
    # find edges with length 0 if there are any
    zero_edges <- tree$edge.length == 0
    # find the second node for these edges, if this is a pendant branch
    # then the second node will be numbered between 1 and n for n tips
    zero_second_node <- sort(tree$edge[zero_edges, 2], decreasing = TRUE)[1]
    if (!is.na(zero_second_node) && zero_second_node > length(tree$tip.label)) {
      # if second node is larger than n, this is an internal edge with length 0
      return(FALSE) 
    } else {
      return(TRUE)
    }
  } else {
    return(FALSE)
  }
}
