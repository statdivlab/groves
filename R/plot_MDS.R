#' Plots MDS results
#' Plots MDS results of trees for each distance.
#'
#' @param df A dataframe with the first two MDS coordinates for a set of trees and distances.
#' It should include variables named \code{MDS_x} and \code{MDS_y}. 
#' @param phylogenomic An optional argument, if included gives the index of the dataframe that
#' corresponds to the phylogenomic tree, which will then be noted in the plot. 
#' @param phylogenomic_name An optional argument, gives the name of the \code{phylogenomic} tree.
#' @param other_tree An optional additional tree to plot in a different color. 
#' @param other_name An optional name for \code{other_tree} if it is included.
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
#' @param trees_to_label Optional set of names in \code{tree_names} to label on plot. These are
#' only labeled if \code{use_plotly} is set to \code{FALSE}.
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
plot_MDS <- function(df, phylogenomic = NULL, phylogenomic_name = NULL,
                     other_tree = NULL, other_name = "other tree",
                     group = NULL, title = "MDS of Gene Tree Distances",
                     show_legend = TRUE, legend_lab = NULL, 
                     tree_names = NULL, alpha = 1, use_plotly = FALSE,
                     trees_to_label = NULL, x_axis = 1, y_axis = 2) {
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
      plot <- ggplot(df, aes(x = MDS_x, y = MDS_y, label = Name)) + 
        geom_point(alpha = alpha) 
    # Grouping variable 
    } else {
      plot <- ggplot(df, aes(x = MDS_x, y = MDS_y, label = Name, color = group)) + 
        geom_point(alpha = alpha) +
        labs(color = ifelse(is.null(legend_lab), "Group", legend_lab)) +
        theme(legend.position = legend_pos)
    }
  # Including phylogenomic tree 
  } else {
    if (is.null(phylogenomic_name)) {
      phylogenomic_name = "phylogenomic"
    }
    # if phylogenomic tree or other tree is included, make tree_type variable 
    df$tree_type <- rep("gene tree", nrow(df))
    df$tree_type[phylogenomic] <- "phylogenomic"
    df$tree_type[other_tree] <- "other"
    df$tree_type <- factor(df$tree_type, 
                           levels = c("gene tree", 
                                      "phylogenomic", 
                                      "other"))
    
    other_df <- dplyr::filter(df, tree_type != "gene tree")
    
    if (is.null(group)) {
      plot <- ggplot(df, aes(x = MDS_x, y = MDS_y, color = tree_type, label = Name)) + 
        geom_point(alpha = alpha) + 
        labs(color = "Tree Type") +
        geom_point(data = other_df) +
        scale_color_manual(values = c("black", "red", "green"),
                           labels = c("gene tree",
                                      latex2exp::TeX(phylogenomic_name),
                                      latex2exp::TeX(other_name))) 
    # Grouping variable 
    } else {
      plot <- ggplot(df, aes(x = MDS_x, y = MDS_y, label = Name, color = group,
                             shape = tree_type)) + 
        geom_point(alpha = alpha) +
        labs(color = ifelse(is.null(legend_lab), "Group", legend_lab),
             shape = "Tree Type") +
        theme(legend.position = legend_pos) +
        geom_point(data = other_df, color = "black") +
        scale_shape_discrete(labels = c("gene tree",
                                        latex2exp::TeX(phylogenomic_name),
                                        latex2exp::TeX(other_name)))
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
    # if plotly not used, and trees to label are given, add those to plot 
    if (!is.null(trees_to_label)) {
      label_df <- dplyr::filter(df, Name %in% trees_to_label) 
      full_plot <- full_plot + 
        ggrepel::geom_text_repel(data = label_df, aes(label = Name), color = "black")
    }
    return(full_plot)
  }
}
