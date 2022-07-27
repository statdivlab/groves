#' List long branch tips
#' Returns a list of tips that are children of the longest branch
#'
#' @param tree A phylo or multiphylo object.
#'
#' @return A list of tips that are children of the longest branch of a single tree (for a phylo object) and
#' a list of lists of tips that are children of the longest branch of each tree (for a multiPhylo object).
#' @export
list_long_branch_tips <- function(tree) {
  #num <- vector(length = length(tree))
  if (class(tree) == "phylo") {
    long_branch <- which.max(tree$edge.length)
    branch_node <- tree$edge[long_branch,2]
    if (branch_node <= length(tree$tip.label)) {
      tips <- list(tree$tip.label[branch_node])
    } else {
      ind <- branch_node - length(tree$tip.label)
      tips <- list(adephylo::listTips(tree)[[ind]])
    }
  } else if (class(tree) == "multiPhylo") {
    tips <- list()
    for (i in 1:length(tree)) {
      temp_tree <- tree[[i]]
      long_branch <- which.max(temp_tree$edge.length)
      branch_node <- temp_tree$edge[long_branch,2]
      if (branch_node <= length(temp_tree$tip.label)) {
        tips[[i]] <- list(temp_tree$tip.label[branch_node])
      } else {
        ind <- branch_node - length(temp_tree$tip.label)
        tips[[i]] <- list(adephylo::listTips(temp_tree)[[ind]])
      }
    }
  } else {tips = NA}
  return(tips)
}
