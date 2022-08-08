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
#' path <- system.file("txt", "gene_trees.txt", package = "groves")
#' gene_trees <- ape::read.tree(path)
#' count_topologies(gene_trees)$count
#'
#' @export
count_topologies <- function(trees, names = NULL) {
  # compute Robinson Foulds distances between all trees
  RF_distances <- as.matrix(phangorn::RF.dist(trees))
  RF_zero <- RF_distances == 0
  top_df <- data.frame(tree = 1:length(trees), topology = rep(0, length(trees)))
  for (i in 1:length(trees)) {
    for (j in 1:length(trees)) {
      # TRUE if same topology (always true on diagonal), FALSE if different
      val <- RF_zero[i,j]
      if (val == TRUE) {
        if (top_df[i,2] == 0) {
          # if same topology, save the topology of tree i as the same as tree j
          # this means that if multiple trees have the same topology, they 
          # will all be saved by the index of the last tree with this topology
          top_df[i,2] <- j
        }
      }
    }
  }
  # add tree names if necessary
  if (!is.null(names)) {
    top_df$tree_name <- names
  }
  # make dataframe with count for each topology
  count_df <- top_df %>%
    group_by(topology) %>%
    mutate(count = n()) %>%
    ungroup()
  return(list(count = length(unique(count_df$topology)), df = count_df))
}
