#' Plot logmap
#' Plot logmap coordinates using the first two principal components.
#'
#' @param vectors A matrix of logmap vectors for each tree.
#' @param base_name The name of the base tree in the log map.
#' @param gene_names An optional list of gene names. If no names are given, numbers will be used
#' to identify trees.
#' @param col An optional list of gene attributes to color the points of the scatterplot by.
#' @param col_name An optional name for the color attribute.
#' @param show_legend If true, the color legend is shown, if false it is hidden.
#' @param cons_exists True if plot includes a consensus tree, false if not.
#'
#' @return A ggplot object.
#'
#'
#' @export
plot_logmap <- function(vectors, base_name, gene_names = NULL, col = NULL,
                        col_name = NULL, show_legend = TRUE, cons_exists = TRUE) {
  if (cons_exists) {
    pca <- stats::prcomp(vectors, rank. = 2)
    if (is.null(gene_names)) {
      gene_names <- 1:(nrow(vectors)-1)
    }
    n <- length(gene_names) + 1
    pca_gene <- data.frame(dim1 = pca$x[2:n,1],
                           dim2 = pca$x[2:n,2],
                           name = gene_names)
    pca_consen <- data.frame(dim1 = pca$x[1,1],
                             dim2 = pca$x[1,2],
                             name = paste0(base_name," Tree"))
    title <- paste0('Log Map of Gene Trees with Respect to ', base_name, ' Tree')
    pca_plot <- ggplot(pca_gene, aes(x = dim1, y = dim2, Gene = name)) +
      geom_point(color = "black") +
      geom_point(data = pca_consen, color = "red") +
      ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5, size = 12)) +
      xlab("First Principal Component") + ylab("Second Principal Component")
    if (!is.null(col)) {
      if (is.null(col_name)) {show_legend = FALSE}
      legend_pos = "none"
      if (show_legend) { legend_pos = "right"}
      pca_gene$gradient <- col
      if (is.numeric(col)) {
        pca_consen$gradient <- NA
        pca_plot <- ggplot(pca_gene, aes(x = dim1, y = dim2, Gene = name, color = gradient)) +
          geom_point() +
          geom_point(data = pca_consen, color = "red") +
          ggtitle(title) + labs(color = col_name) +
          theme(plot.title = element_text(hjust = 0.5, size = 12)) +
          xlab("First Principal Component") + ylab("Second Principal Component")
      } else {
        pca_consen$gradient <- NA
        pca_plot <- ggplot(pca_gene, aes(x = dim1, y = dim2, Gene = name, color = gradient)) +
          geom_point() +
          geom_point(data = pca_consen, color = "red") +
          ggtitle(title) + labs(color = col_name) +
          theme(plot.title = element_text(hjust = 0.5, size = 12)) +
          xlab("First Principal Component") + ylab("Second Principal Component") +
          scale_color_manual(values = c("black","blue","green","purple","yellow","orange","pink",
                                        "brown","aquamarine","darkgreen","darkgray","plum"))

      }
    }
    if (!is.null(other_cons_name)) {
      pca_consen1 <- data.frame(dim1 = pca$x[n+1,1],
                                dim2 = pca$x[n+1,2],
                                name = paste0(other_cons_name," Tree"))
      pca_plot <- ggplot(pca_gene, aes(x = dim1, y = dim2, Gene = name)) +
        geom_point(aes(color = "black")) +
        geom_point(data = pca_consen1, aes(color = "green")) +
        geom_point(data = pca_consen, aes(color = "red")) +
        ggtitle(title) +
        scale_colour_manual(name = 'Tree Type',
                            values =c('black'='black','green' = 'green','red'='red'),
                            labels = c('Gene Tree',other_cons_name, base_name))
      theme(plot.title = element_text(hjust = 0.5, size = 12)) +
        xlab("First Principal Component") + ylab("Second Principal Component")
    }
    return(list(df = rbind(pca_gene, pca_consen), plot = pca_plot))
  } else {
    pca <- stats::prcomp(vectors, rank. = 2)
    if (is.null(gene_names)) {
      gene_names <- 1:(nrow(vectors)-1)
    }
    n <- length(gene_names) + 1
    pca_gene <- data.frame(dim1 = pca$x[,1],
                           dim2 = pca$x[,2],
                           name = gene_names)
    title <- paste0('Log Map of Gene Trees with Respect to ', base_name, ' Tree')
    pca_plot <- ggplot(pca_gene, aes(x = dim1, y = dim2, Gene = name)) +
      geom_point(color = "black") +
      ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5, size = 12)) +
      xlab("First Principal Component") + ylab("Second Principal Component")
    if (!is.null(col)) {
      if (is.null(col_name)) {show_legend = FALSE}
      legend_pos = "none"
      if (show_legend) { legend_pos = "right"}
      pca_gene$gradient <- col
      if (is.numeric(col)) {
        pca_plot <- ggplot(pca_gene, aes(x = dim1, y = dim2, Gene = name, color = gradient)) +
          geom_point() +
          ggtitle(title) + labs(color = col_name) +
          theme(plot.title = element_text(hjust = 0.5, size = 12)) +
          xlab("First Principal Component") + ylab("Second Principal Component")
      } else {
        pca_plot <- ggplot(pca_gene, aes(x = dim1, y = dim2, Gene = name, color = gradient)) +
          geom_point() +
          ggtitle(title) + labs(color = col_name) +
          theme(plot.title = element_text(hjust = 0.5, size = 12)) +
          xlab("First Principal Component") + ylab("Second Principal Component") +
          scale_color_manual(values = c("black","blue","green","purple","yellow","orange","pink",
                                        "brown","aquamarine","darkgreen","darkgray","plum"))
      }
    }
    return(list(df = pca_gene, plot = pca_plot))
  }
}
