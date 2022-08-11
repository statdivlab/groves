#' Plot gene tree support
#' Plot a tree with gene tree support values on each edge.
#'
#' @param main_tree A rooted tree to plot edge information for.
#' @param support A list of values for each branch of main_tree.
#' @param lab_size The size of the tip labels, default is 2.
#' @param supp_size The size of the support labels, default is 2.
#' @param xlim_max The length of the x axis.
#' @param hjust Horizontal adjustment for support labels.
#'
#' @return A ggtree object.
#'
#' @examples 
#' path <- system.file("txt", "small_tree_set.txt", package = "groves")
#' trees <- ape::read.tree(path)
#' support <- check_support(trees[[1]], trees)$branch_support
#' plot_gene_support(main_tree = trees[[1]], support = support, xlim_max = 4)
#'
#' @import ggtree
#'
#' @export
plot_gene_support <- function(main_tree, support,
                              lab_size = 2, supp_size = 2, xlim_max = 0.5, hjust = 1) {
  # second node in edge for all internal edges
  edge_second_node <- (length(main_tree$tip.label) +
                        main_tree$Nnode - length(support) + 1):(
                          length(main_tree$tip.label) +
                            main_tree$Nnode)
  edge <- data.frame(main_tree$edge, support = rep(NA, nrow(main_tree$edge)))
  ind <- match(edge_second_node, edge$X2)
  edge$support[ind] <- support
  colnames(edge)=c("parent", "node", "support")
  base <- main_tree %>% ggtree + geom_tiplab(size = lab_size) + ggplot2::xlim(0, xlim_max)
  plot <- base %<+% edge + geom_text(aes(x=branch, label = support, color = support),
                                     vjust = -.3, hjust = hjust, size = supp_size) +
    scale_colour_gradient(low = "red", high = "blue") +
    theme(legend.position = "none")
  return(plot)
}
