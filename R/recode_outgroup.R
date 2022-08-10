#' Recodes outgroup edge.
#' Recodes the branch going to an outgroup to the value provided.
#'
#' @param tree The tree or set of trees of interest.
#' @param outgroup The name of the outgroup in the tree of interest.
#' @param value The value to make the outgroup edge. If not given, defaults to 1.
#'
#' @return The tree or set of trees with the outgroup edge changed to the value provided.
#'
#' @examples 
#' path <- system.file("txt", "small_tree_set.txt", package = "groves")
#' tree_set <- ape::read.tree(path)
#' new_tree_set <- recode_outgroup(tree = tree_set, outgroup = "t1")
#'
#' @export
recode_outgroup <- function(tree, outgroup, value = 1) {
  # if tree input is a single tree 
  if (class(tree) == "phylo") {
    # check the outgroup exists in tree 
    outgroup_ind <- which(tree$tip.label == outgroup)
    if (length(outgroup_ind) == 0) {
      stop("Please provide outgroup included on input tree.")
    }
    # find branch going to outgroup
    edge_ind <- which(tree$edge[, 2] == outgroup_ind)
    new_tree <- tree
    # make new tree with outgroup branch recoded to provided value
    new_tree$edge.length[edge_ind] <- value
    return(new_tree)
  # if tree input is a set of trees 
  } else if (class(tree) == "multiPhylo") {
    # check that outgroup exists in first tree 
    outgroup_ind <- which(tree[[1]]$tip.label == outgroup)
    if (length(outgroup_ind) == 0) {
      stop("The provided outgroup does not exist in tree 1 of the set.")
    }
    # recode outgroup
    edge_ind <- which(tree[[1]]$edge[, 2] == outgroup_ind)
    new_tree <- tree[[1]]
    new_tree$edge.length[edge_ind] <- value
    new_trees <- new_tree 
    # do the same for the rest of the tree set 
    for (i in 2:length(tree)) {
      outgroup_ind <- which(tree[[i]]$tip.label == outgroup)
      if (length(outgroup_ind) == 0) {
        stop(paste0("The provided outgroup does not exist in tree ", i, " of the set."))
      }
      edge_ind <- which(tree[[i]]$edge[, 2] == outgroup_ind)
      new_tree <- tree[[i]]
      new_tree$edge.length[edge_ind] <- value
      new_trees <- c(new_trees, new_tree) 
    }
    return(new_trees)
  } else {
    stop("Please submit a tree (phylo object) or set of trees (multiPhylo object).")
  }
}
