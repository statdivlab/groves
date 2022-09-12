#' Plots PCA of log map results
#' Plots PCA of log map results of trees for each distance.
#'
#' @param vectors A matrix with a row for the log map coordinate of each tree. The output
#' from compute_logmap.
#' @param phylogenomic An optional argument, if included gives the index of the \code{vectors} 
#' matrix that corresponds to the phylogenomic tree, which will then be noted in the plot. 
#' @param group An optional vector to color points by. This should be same length as the 
#' number of rows in \code{vector}.
#' @param title An optional parameter to change the plot title.
#' @param show_legend A boolean about whether or not to show a legend for grouping variable.
#' @param legend_lab A label for the legend for the grouping variable. Enter "" to hide
#' the legend label.
#' @param tree_names An optional variable to label trees by. This should be same length as the 
#' number of rows in \code{vector}.
#' @param alpha Transparency of the points, defaults to 1 (fully opaque).
#' @param use_plotly If true, the output will be a plotly object, with labels
#' when points are moused over, if false the output will be a ggplot object.
#'
#' @return A ggplot2 object.
#' @import ggplot2
#' 
#' @examples 
#' path <- paste0(system.file("txt", package = "groves"), "/")
#' lm_vectors <- compute_logmap(tree_paths = paste0(path, "tree", 1:3, ".txt"),
#'                tree_names = c("tree1", "tree2", "tree3"))$vectors
#' names <- paste0("tree", 1:3)
#' med_branch <- c(1, 5, 2)
#' plot_logmap(vectors = lm_vectors, phylogenomic = 1, group = med_branch, title = "PCA plot",
#'          show_legend = TRUE, legend_lab = "Branch median", tree_names = names,
#'          alpha = 0.9, use_plotly = FALSE)
#'
#' @export
plot_logmap <- function(vectors, phylogenomic = NULL, group = NULL,
                     title = "MDS of Gene Tree Distances",
                     show_legend = TRUE, legend_lab = NULL, 
                     tree_names = NULL, alpha = 1, use_plotly = FALSE) {
  ### start by organizing df 
  # get first two pca coordinates
  pca <- stats::prcomp(vectors, rank. = 2)
  df <- data.frame(PC1 = pca$x[ ,1],
                   PC2 = pca$x[ ,2])
  if (is.null(tree_names)) {
    # label trees by number if no names given 
    df$Name <- 1:nrow(df)
  } else {
    df <- dplyr::mutate(df, "Name" = tree_names)
  }
  if (!is.null(group)) {
    df <- dplyr::mutate(df, "group" = group)
    if (!is.numeric(df$group)) {
      df$group <- as.factor(df$group)
    } 
    legend_pos = "right"
    if (!show_legend) { legend_pos = "none"}
  }
  
  ### plotting 
  # No phylogenomic tree 
  if (is.null(phylogenomic)) {
    # No grouping variable 
    if (is.null(group)) {
      plot <- ggplot(df, aes(x = PC1, y = PC2, label = Name)) + 
        geom_point(alpha = alpha) 
      # Grouping variable 
    } else {
      plot <- ggplot(df, aes(x = PC1, y = PC2, label = Name, color = group)) + 
        geom_point(alpha = alpha) +
        labs(color = ifelse(is.null(legend_lab), "Group", legend_lab)) +
        theme(legend.position = legend_pos)
    }
    # Including phylogenomic tree 
  } else {
    # if phylogenomic tree is included, make separate phylogenomic df 
    genom_df <- df[phylogenomic, ]
    df$tree_type <- rep("gene tree", nrow(df))
    df$tree_type[phylogenomic] <- "phylogenomic"
    if (is.null(group)) {
      plot <- ggplot(df, aes(x = PC1, y = PC2, color = tree_type, label = Name)) + 
        geom_point(alpha = alpha) + 
        scale_color_manual(values = c("black", "red")) +
        labs(color = "Tree Type")
      geom_point(data = genom_df, color = "red") 
      # Grouping variable 
    } else {
      plot <- ggplot(df, aes(x = PC1, y = PC2, label = Name, color = group,
                             shape = tree_type)) + 
        geom_point(alpha = alpha) +
        labs(color = ifelse(is.null(legend_lab), "Group", legend_lab),
             shape = "Tree Type") +
        theme(legend.position = legend_pos)
    }
  }
  # add details 
  full_plot <- plot +
    labs(x = 'Principal Component 1',
         y = 'Principal Component 2') +
    ggtitle(title) + 
    theme(plot.title = element_text(hjust = 0.5))
  # use plotly if asked for 
  if (use_plotly) {
    return(plotly::ggplotly(full_plot, tooltip = "Name"))
  } else {
    return(full_plot)
  }
}
