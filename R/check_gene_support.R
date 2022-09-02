#' Check gene support
#' Check the proportion of trees that support splits in a single tree.
#'
#' @param main_tree A tree to check support for.
#' @param trees A set of trees to use to check support for main_tree.
#' @param rooted TRUE if trees are rooted, FALSE otherwise. Defaults to FALSE. 
#'
#' @return A vector of support proportion for each internal node of \code{main_tree}.
#'
#' @examples 
#' path1 <- system.file("txt", "gene_trees.txt", package = "groves")
#' path2 <- system.file("txt", "concat_tree.txt", package = "groves")
#' gene_trees <- ape::read.tree(path1)
#' concat_tree <- ape::read.tree(path2)
#' check_gene_support(concat_tree, gene_trees, rooted = FALSE)
#'
#' @export
check_gene_support <- function(main_tree, trees, rooted = FALSE) {
  # use ape to count partitions in trees set 
  partitions <- ape::prop.clades(main_tree, trees, rooted = rooted)
  # save NA values as 0 
  partitions[is.na(partitions)] <- 0
  return(round(partitions/length(trees), 2))
}
