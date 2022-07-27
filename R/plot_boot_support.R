#' Plot bootstrap support
#' Plot a tree with bootstrap support values on each edge.
#'
#' @param main_tree A tree to plot edge information for.
#' @param support A list of node values for each branch of main_tree.
#' @param lab_size The size of the tip labels, default is 2.
#' @param supp_size The size of the support labels, default is 2.
#' @param xlim_max The length of the x axis.
#' @param hjust Horizontal adjustment for support labels.
#'
#' @return A ggtree object.
#'
#' @import ggtree
#'
#' @export
plot_boot_support <- function(main_tree, support,
                              lab_size = 2, supp_size = 2, xlim_max = 0.5, hjust = 1) {
  edge <- data.frame(main_tree$edge, support = rep(NA, nrow(main_tree$edge)))
  ind <- which(edge$X2 > length(main_tree$tip.label))
  support_prop <- round(as.integer(support)/100, 2)
  edge$support[ind] <- support_prop
  colnames(edge)=c("parent", "node", "support")
  base <- main_tree %>% ggtree + geom_tiplab(size = lab_size) + ggplot2::xlim(0, xlim_max)
  plot <- base %<+% edge + geom_text(aes(x=branch, label = support, color = support),
                                     vjust = -.3, hjust = hjust, size = supp_size) +
    scale_colour_gradient(low = "red", high = "blue") +
    theme(legend.position = "none")
  return(plot)
}
