#' Plots MDS results
#' Plots MDS results of trees for each distance.
#'
#' @param df A dataframe with the first two MDS coordinates for a set of trees and distances.
#' @param consensus An optional argument specifying whether the first tree is a consensus tree, and
#' if so the name of the consensus tree.
#' @param group An optional variable to color points by.
#' @param title An optional paramater to change the plot title.
#' @param x_dim The name of the variable within the dataframe that specifies the value on the x axis.
#' @param y_dim The name of the variable within the dataframe that specifies the value on the y axis.
#' @param show_legend A boolean about whether or not to show a legend.
#' @param legend_lab A label for the legend (NULL if not specified).
#' @param scales_arg An optional argument about whether the scales should be fixed or free. Set to
#' free by default.
#' @param single_method True if the dataframe only includes data from a single distance metric.
#' @param gene_names An optional variable to label genes by.
#'
#' @return A ggplot2 object.
#' @import ggplot2
#' @import dplyr
#'
#'
#' @export
plot_MDS <- function(df, consensus = NULL, group = NULL,
                     title = "MDS of Gene Tree Distances",
                     x_dim = "scale_x_dim", y_dim = "scale_y_dim",
                     show_legend = FALSE, legend_lab = NULL, scales_arg = "free",
                     single_method = FALSE, gene_names = NULL) {
  if (single_method) { df$method = rep("distance",nrow(df))}
  if (is.null(gene_names)) {gene_names = x_dim}
  gene_trees <- df %>% filter(tree_type == "gene")
  cons_tree <- df %>% filter(tree_type != "gene")
  if (is.null(group)) {
    plot <- ggplot(gene_trees, aes(x = get(x_dim), y = get(y_dim), name = get(gene_names))) +
      geom_point(color = "black") +
      geom_point(data = cons_tree, color = "red") +
      facet_wrap(~method, scales = scales_arg) +
      ggtitle(title) +
      xlab("MDS Dimension 1") +
      ylab("MDS Dimension 2") +
      theme(plot.title = element_text(hjust = 0.5))
  }
  else {
    legend_pos = "none"
    if (show_legend) { legend_pos = "right"}
    if (is.numeric(gene_trees[,group])) {
      plot <- ggplot(gene_trees, aes(x = get(x_dim), y = get(y_dim), color = get(group),
                                     name = get(gene_names))) +
        geom_point() +
        #scale_color_manual(values = c("black","blue","green","purple","yellow","orange","pink",
        #                              "brown","aquamarine","darkgreen","darkgray","plum")) +
        geom_point(data = cons_tree, color = "red") +
        facet_wrap(~method, scales = "free") +
        ggtitle(title) +
        xlab("MDS Dimension 1") +
        ylab("MDS Dimension 2") +
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = legend_pos) +
        labs(color = legend_lab)
    } else {
        gene_trees[,group] <- as.factor(gene_trees[,group])
        plot <- ggplot(gene_trees, aes(x = get(x_dim), y = get(y_dim), color = get(group),
                                       name = get(gene_names))) +
        geom_point() +
        scale_color_manual(values = c("black","blue","green","purple","yellow","orange","pink",
                                      "brown","aquamarine","darkgreen","darkgray","plum")) +
        geom_point(data = cons_tree, color = "red") +
        facet_wrap(~method, scales = "free") +
        ggtitle(title) +
        xlab("MDS Dimension 1") +
        ylab("MDS Dimension 2") +
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = legend_pos) +
        labs(color = legend_lab)
    }
  }
  return(plot)
}


