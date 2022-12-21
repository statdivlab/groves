#' Plots MDS results
#' Plots MDS results of trees for each distance.
#'
#' @param df A dataframe with the first two MDS coordinates for a set of trees and distances.
#' It should include variables named \code{MDS1} and \code{MDS2}. 
#' @param phylogenomic An optional argument, if included gives the index of the dataframe that
#' corresponds to the phylogenomic tree, which will then be noted in the plot. 
#' @param group An optional variable to color points by. This should either be a vector the same
#' length as the number of rows in \code{df} or a name in quotes of a variable in \code{df}.
#' @param title An optional parameter to change the plot title.
#' @param show_legend A boolean about whether or not to show a legend for grouping variable.
#' @param legend_lab A label for the legend for the grouping variable. Enter "" to hide
#' the legend label.
#' @param tree_names An optional variable to label trees by. This should either be a vector
#' the same length as the number of rows in \code{df} or a name in quotes of a variable in 
#' \code{df}.
#' @param alpha Transparency of the points, defaults to 1 (fully opaque).
#' @param use_plotly If true, the output will be a plotly object, with labels
#' when points are moused over, if false the output will be a ggplot object.
#' @param x_axis Dimension number to use on the x-axis. Defaults to 1.
#' @param y_axis Dimension number to use on the y-axis. Defaults to 2.
#'
#' @return A ggplot2 object.
#' @import ggplot2
#' 
#' @examples 
#' trees_path <- system.file("txt", "small_tree_set.txt", package = "groves")
#' plot_df <- compute_MDS(trees_path = trees_path, tree_names = paste0("tree", 1:3))$df
#' plot_df$names <- paste0("tree", 1:3)
#' plot_df$type <- c("ribosomal", "ribosomal", "other")
#' plot_df$med_branch <- c(1, 5, 2)
#' plot_MDS(df = plot_df, phylogenomic = 1, group = "med_branch", title = "MDS plot",
#'          show_legend = TRUE, legend_lab = "Branch median", tree_names = "names",
#'          alpha = 0.9, use_plotly = FALSE)
#'
#' @export
plot_MDS <- function(df, phylogenomic = NULL, group = NULL,
                     title = "MDS of Gene Tree Distances",
                     show_legend = TRUE, legend_lab = NULL, 
                     tree_names = NULL, alpha = 1, use_plotly = FALSE,
                     x_axis = 1, y_axis = 2) {
  ### start by organizing df 
  if (is.null(tree_names)) {
    # label trees by number if no names given 
    df$Name <- 1:nrow(df)
  } else {
    if (length(tree_names) > 1) {
      # if tree_names is a vector, add it to df with name `Name`
      df <- dplyr::mutate(df, "Name" = tree_names)
    } else {
      # if tree_names is a variable in df, copy it into variable with name `Name`
      df <- dplyr::mutate(df, "Name" = df[, tree_names])
    }
  }
  if (!is.null(group)) {
    if (length(group) > 1) {
      # if group is a vector, add it to df with name `group`
      df <- dplyr::mutate(df, "group" = group)
    } else {
      # if group is a variable in df, copy into variable with name `group`
      df <- dplyr::mutate(df, "group" = df[, group])
    }
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
      plot <- ggplot(df, aes(x = MDS1, y = MDS2, label = Name)) + 
        geom_point(alpha = alpha) 
    # Grouping variable 
    } else {
      plot <- ggplot(df, aes(x = MDS1, y = MDS2, label = Name, color = group)) + 
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
      plot <- ggplot(df, aes(x = MDS1, y = MDS2, color = tree_type, label = Name)) + 
        geom_point(alpha = alpha) + 
        scale_color_manual(values = c("black", "red")) +
        labs(color = "Tree Type")
        geom_point(data = genom_df, color = "red") 
    # Grouping variable 
    } else {
      plot <- ggplot(df, aes(x = MDS1, y = MDS2, label = Name, color = group,
                             shape = tree_type)) + 
        geom_point(alpha = alpha) +
        labs(color = ifelse(is.null(legend_lab), "Group", legend_lab),
             shape = "Tree Type") +
        theme(legend.position = legend_pos)
    }
  }
  # add details 
  full_plot <- plot +
    labs(x = paste0('Dimension ', x_axis),
         y = paste0('Dimension ', y_axis)) +
    ggtitle(title) + 
    theme(plot.title = element_text(hjust = 0.5))
  # use plotly if asked for 
  if (use_plotly) {
    return(plotly::ggplotly(full_plot, tooltip = "Name"))
  } else {
    return(full_plot)
  }
}
