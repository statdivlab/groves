#' Plots PCA of log map results
#' Plots PCA of log map results of trees for each distance.
#'
#' @param vectors A matrix with a row for the log map coordinate of each tree. The output
#' from compute_logmap.
#' @param phylogenomic An optional argument, if included gives the index of the \code{vectors} 
#' matrix that corresponds to the phylogenomic tree, which will then be noted in the plot. 
#' @param phylogenomic_name An optional argument, gives the name of the \code{phylogenomic} tree.
#' @param group An optional vector to color points by. This should be same length as the 
#' number of rows in \code{vector}.
#' @param other_tree An optional additional tree to plot in a different color. 
#' @param other_name An optional name for \code{other_tree} if it is included.
#' @param ignore_in_pca The index of any rows in \code{vectors} that should be ignored when
#' running PCA. This can be used to add a new point to an existing plot. 
#' @param title An optional parameter to change the plot title.
#' @param show_legend A boolean about whether or not to show a legend for grouping variable.
#' @param legend_lab A label for the legend for the grouping variable. Enter "" to hide
#' the legend label.
#' @param tree_names An optional variable to label trees by. This should be same length as the 
#' number of rows in \code{vector}.
#' @param alpha Transparency of the points, defaults to 1 (fully opaque).
#' @param use_plotly If true, the output will be a plotly object, with labels
#' when points are moused over, if false the output will be a ggplot object.
#' @param trees_to_label Optional set of names in \code{tree_names} to label on plot. These are
#' only labeled if \code{use_plotly} is set to \code{FALSE}.
#' @param x_axis Principal component number to use on the x-axis. Defaults to 1.
#' @param y_axis Principal component number to use on the y-axis. Defaults to 2.
#'
#' @return A ggplot2 object.
#' @import ggplot2
#' 
#' @examples 
#' path <- paste0(system.file("txt", package = "groves"), "/")
#' lm_vectors <- compute_logmap(tree_paths = paste0(path, "tree", 1:4, ".txt"),
#'                tree_names = c("tree1", "tree2", "tree3", "tree4"))$vectors
#' names <- paste0("tree", 1:4)
#' med_branch <- c(1, 5, 2, 4)
#' plot_logmap(vectors = lm_vectors, phylogenomic = 1, phylogenomic_name = NULL, 
#'            group = med_branch, title = "PCA plot", show_legend = TRUE, 
#'            legend_lab = "Branch median", tree_names = names,
#'            alpha = 0.9, use_plotly = FALSE, trees_to_label = "tree1")
#'
#' @export
plot_logmap <- function(vectors, phylogenomic = NULL, phylogenomic_name = NULL, 
                        group = NULL, other_tree = NULL, other_name = "other tree", 
                        ignore_in_pca = NULL, title = "PCA of Trees",
                        show_legend = TRUE, legend_lab = NULL, 
                        tree_names = NULL, alpha = 1, 
                        trees_to_label = NULL, use_plotly = FALSE,
                        x_axis = 1, y_axis = 2) {
  ### start by organizing df 
  # get first two pca coordinates
  if (is.null(ignore_in_pca)) {
    pca <- stats::prcomp(vectors)
    df <- data.frame(pc_x = pca$x[, x_axis],
                     pc_y = pca$x[, y_axis])
  } else {
    pca <- stats::prcomp(vectors[-ignore_in_pca, ])
    pc_x <- rep(NA, nrow(vectors))
    pc_y <- rep(NA, nrow(vectors))
    pc_x[-ignore_in_pca] <- pca$x[, x_axis]
    pc_y[-ignore_in_pca] <- pca$x[, y_axis]
    centered_matrix <- t(matrix(pca$center, nrow = ncol(vectors), ncol = length(ignore_in_pca)))
    new_pcs <- (vectors[ignore_in_pca, ] - centered_matrix) %*% pca$rotation
    pc_x[ignore_in_pca] <- new_pcs[, x_axis]
    pc_y[ignore_in_pca] <- new_pcs[, y_axis]
    df <- data.frame(pc_x = pc_x,
                     pc_y = pc_y)
  }
  
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
  if (is.null(phylogenomic) & is.null(other_tree)) {
    # No grouping variable 
    if (is.null(group)) {
      plot <- ggplot(df, aes(x = pc_x, y = pc_y, label = Name)) + 
        geom_point(alpha = alpha) 
      # Grouping variable 
    } else {
      plot <- ggplot(df, aes(x = pc_x, y = pc_y, label = Name, color = group)) + 
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
      plot <- ggplot(df, aes(x = pc_x, y = pc_y, color = tree_type, label = Name)) + 
        geom_point(alpha = alpha) + 
        labs(color = "Tree Type") +
        geom_point(data = other_df) +
        #scale_color_discrete(labels = c("gene tree",
        #                                latex2exp::TeX(phylogenomic_name),
        #                                latex2exp::TeX(other_name))) +
        scale_color_manual(values = c("black", "red", "green"),
                           labels = c("gene tree",
                                      latex2exp::TeX(phylogenomic_name),
                                      latex2exp::TeX(other_name))) 
    # Grouping variable 
    } else {
      plot <- ggplot(df, aes(x = pc_x, y = pc_y, label = Name, color = group,
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
    labs(x = 'Principal Component 1',
         y = 'Principal Component 2') +
    ggtitle(title) + 
    theme(plot.title = element_text(hjust = 0.5))
  # use plotly if asked for 
  if (use_plotly) {
    full_plot <- plotly::ggplotly(full_plot, tooltip = "Name")
  } else {
    # if plotly not used, and trees to label are given, add those to plot 
    if (!is.null(trees_to_label)) {
      label_df <- dplyr::filter(df, Name %in% trees_to_label) 
      full_plot <- full_plot + 
        ggrepel::geom_text_repel(data = label_df, aes(label = Name), color = "black")
    }
  }
  # save dataframes for PC 1 and PC2
  pc_df <- data.frame(pc_x = df$pc_x,
                      pc_y = df$pc_y,
                      Name = df$Name,
                      index = 1:nrow(df)) 
  pc_x_df <- dplyr::arrange(pc_df, dplyr::desc(abs(pc_x)))
  pc_y_df <- dplyr::arrange(pc_df, dplyr::desc(abs(pc_y)))
  return(list(plot = full_plot, pc_x_info = pc_x_df, pc_y_info = pc_y_df))
}
