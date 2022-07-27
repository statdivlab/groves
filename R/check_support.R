#' Check support
#' Check the proportion of trees that support splits in a single tree.
#'
#' @param main_tree A tree to check support for.
#' @param trees A set of trees to use to check support for main_tree.
#'
#' @return A list with an average support value (averaged over all splits in all trees) and
#' a support proportion for each internal branch of main_tree.
#'
#' @export
check_support <- function(main_tree, trees) {
  support <- TreeTools::SplitFrequency(main_tree, trees)
  prop <- sum(support)/(length(trees)*length(support))
  return(list(support_prop = prop, branch_support = support))
}
