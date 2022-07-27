#' Plot node value
#' Plot a tree with node values on each internal node.
#'
#' @param main_tree A tree to check support for.
#' @param node_lab A list of node values for each internal node of main_tree.
#' @param n_trees The number of trees used to compute \code{node_lab}.
#' @param rooted A boolean, TRUE if the main tree is rooted
#' @param lab_size The size of the tip labels, default is 2.
#' @param supp_size The size of the support labels, default is 2.
#' @param xlim_max The length of the x axis.
#'
#' @return A ggtree object.
#'
#' @import ggtree
#'
#' @export
plot_node_value <- function(main_tree, node_lab, n_trees, rooted = TRUE,
                         lab_size = 2, supp_size = 2, xlim_max = 0.5) {
  n_tips <- length(main_tree$tip.label)
  num <- n_tips + 2
  if (!rooted) {num <- n_tips+1}
  #support_val <- round(c(rep(n_trees,num),branch_support)/n_trees,2)
  node_val <- round(c(rep(NA,num),node_lab)/n_trees,2)
  node_lab <- paste0(node_val)
  plot <- main_tree %>%
    ggtree +
    geom_tiplab(size = lab_size) +
    geom_text2(aes(subset=!isTip, label=node_lab, color = node_val),
               hjust=1, vjust = -0.7, size = supp_size) +
    scale_colour_gradient(low = "red", high = "blue") +
    theme(legend.position = 'none') +
    ggplot2::xlim(0, xlim_max)
  return(plot)
}
