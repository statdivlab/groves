#' Plot support
#' Plot a tree with support values for each internal node. These can be gene tree 
#' support or bootstrap support values. 
#'
#' @param main_tree A rooted tree to plot edge information for.
#' @param support A list of values for each internal node of main_tree. Each value should be
#' a proportion between 0 and 1. 
#' @param support_type Enter \code{"gene"} if \code{support} are gene tree support values,
#' \code{"boot"} if \code{support} are bootstrap support values, and \code{"other"} if 
#' \code{support} values are neither. Defaults to \code{"gene"}.
#' @param lab_size The size of the tip labels, default is 2. Set this to 0 to suppress
#' tip labels.
#' @param supp_size The size of the support labels, default is 2.
#' @param xlim_max The length of the x axis.
#' @param hjust Horizontal adjustment for support labels.
#' @param title An optional parameter to change the plot title. Enter NULL to remove
#' the title. 
#' @param show_legend A boolean about whether or not to show a legend for support.
#' @param color_branch If TRUE, each branch is colored with the support of the internal node
#' it leads to. If FALSE, branch support is displayed with a number between 0 and 1.
#'
#' @return A ggtree object. Note this function will also provide a warning about rows
#' containing missing values if \code{color_branch = TRUE}. These rows correspond with
#' the branches that lead to tips, which all have 100% support. In the output ggree 
#' object, trees will be labeled or colored by support values, and branches with support
#' values equal to zero will be shown with dashed lines.
#'
#' @examples 
#' trees <- ape::rmtree(100, 5, rooted = TRUE)
#' support <- check_gene_support(trees[[1]], trees[2:100], rooted = TRUE)
#' plot_support(main_tree = trees[[1]], support = support, xlim_max = 2)
#' 
#' @import ggtree
#'
#' @export
plot_support <- function(main_tree, support, support_type = "gene", lab_size = 2,
                              supp_size = 2, xlim_max = 0.5, hjust = 1, 
                              color_branch = FALSE, show_legend = TRUE, title = "") {
  if (title == "") {
    if (support_type == "gene") {
      title <- "Gene Tree Support"
    } else if (support_type == "boot") {
      title <- "Bootstrap Support"
    } else if (support_type == "other") {
      title <- "Support"
    } else {
      stop("Please enter 'gene', 'boot', or 'other' for support_type.")
    }
  }
  # save support values as part of tree object with treeio::as.treedata 
  tree <- treeio::as.treedata(main_tree, support)
  # determine if legend is shown
  legend_pos = "right"
  if (!show_legend) { legend_pos = "none"}
  # add support values and colors 
  col.range <- c(0, 1)
  
  # plot with support as a number on each branch leading to an internal node
  if (!color_branch) {
    plot <- tree %>% ggtree(aes(label = bootstrap,
                                linetype = (bootstrap == 0 & !is.na(bootstrap)))) + 
      geom_text(aes(color = bootstrap), vjust = -0.3, hjust = hjust, size = supp_size) +
      scale_color_gradientn(colors = RColorBrewer::brewer.pal(11, "Spectral")[c(1:4,8:11)], limits = col.range) 
  # plot with branches leading to an internal node colored based on support values 
  } else {
    plot <- tree %>% ggtree(aes(label = bootstrap, color = bootstrap,
                                linetype = (bootstrap == 0 & !is.na(bootstrap)))) + 
      scale_color_gradientn(colors = RColorBrewer::brewer.pal(11, "Spectral")[c(1:5,7:11)], limits = col.range) 
  }
  full_plot <- plot + geom_tiplab(size = lab_size, color = "black") + 
    ggplot2::xlim(0, xlim_max) +
    labs(color = "Support",
         title = title) + 
    guides(linetype = "none") +
    theme(legend.position = legend_pos, 
          plot.title = element_text(hjust = 0.5))
  return(full_plot)
}
