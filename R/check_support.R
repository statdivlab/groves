#' Check support
#' Check the proportion of trees that support splits in a single tree.
#'
#' @param main_tree A tree to check support for.
#' @param trees A set of trees to use to check support for main_tree.
#'
#' @return A list with an average support value (averaged over all splits in all trees) and
#' a support proportion for each internal branch of main_tree.
#'
#' @examples 
#' path1 <- system.file("txt", "gene_trees.txt", package = "groves")
#' path2 <- system.file("txt", "concat_tree.txt", package = "groves")
#' gene_trees <- ape::read.tree(path1)
#' concat_tree <- ape::read.tree(path2)
#' check_support(concat_tree, gene_trees)
#'
#' @export
check_support <- function(main_tree, trees) {
  # use function from TreeTools to return number of splits in main tree that appear
  # in set trees
  support <- TreeTools::SplitFrequency(main_tree, trees)
  prop <- round(sum(support)/(length(trees)*length(support)), 2)
  return(list(support_prop = prop, branch_support = round(support/length(trees), 2)))
}
