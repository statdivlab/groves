#' Recode long branches
#' Returns a phylo or multiphylo object with long branches recoded.
#'
#' @param trees A phylo or multiphylo object.
#' @param branch_length A non-negative length to recode long branches to.
#'
#' @return A phylo or multiphylo object containing transformed trees with long branch
#' lengths recoded.
#' @export
recode_long_branch <- function(trees, branch_length) {
  if (class(trees) == "phylo") {
    long_branch <- which.max(trees$edge.length)
    trees$edge.length[long_branch] <- branch_length
  } else if (class(trees) == "multiPhylo") {
    for (i in 1:length(trees)) {
      temp_tree <- trees[[i]]
      long_branch <- which.max(temp_tree$edge.length)
      trees[[i]]$edge.length[long_branch] <- branch_length
    }
  }
  return(trees)
}
