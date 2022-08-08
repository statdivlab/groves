#' Count topologies
#' Count number of distinct topologies present in group of phylogenetic trees on the same set of
#' tips.
#'
#' @param trees A set of trees.
#' @param names An optional set of tree names.
#'
#' @return A list containing the number of distinct topologies and a dataframe with topology
#' information.
#'
#' @importFrom ape read.tree
#'
#' @examples 
#' gene_trees <- read.tree("data/gene_trees.txt")
#' count_topologies(gene_trees)
#'
#' @export
count_topologies <- function(trees, names = NULL) {
  RF_distances <- as.matrix(phangorn::RF.dist(trees))
  RF_zero <- RF_distances == 0
  top_df <- data.frame(tree = 1:length(trees), topology = rep(0, length(trees)))
  for (i in 1:length(trees)) {
    for (j in 1:length(trees)) {
      val <- RF_zero[i,j]
      if (val == TRUE) {
        if (top_df[i,2] == 0) {
          top_df[i,2] <- j
        }
      }
    }
  }
  if (!is.null(names)) {
    top_df$tree_name <- names
  }
  count_df <- top_df %>%
    group_by(topology) %>%
    mutate(count = n()) %>%
    ungroup()
  return(list(count = length(unique(count_df$topology)), df = count_df))
}
