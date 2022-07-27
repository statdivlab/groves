#' Geodesic implementation
#' Calculates the geodesic distance between each tree
#'
#' @param tree_path The file path to a txt file containing phylogenetic trees in a multiPhylo object.
#' @param nf The number of axes in the MDS to keep.
#' @param mds_package The default is to run classical MDS using ade4. To change this to metric MDS using the
#' package smacof, use the argument mds_package = "smacof".
#' @param names Optional tree names to use.
#' @param consensus Optional name of the consensus tree.
#'
#' @return A list containing an object 'dist_mat' containing geodesic distances between each phylogenetic tree
#' in the multiPhylo object, and an object 'df' containing the desired number of axes in the MDS of the trees.
#' @export
geodesic_MDS <- function(tree_path, nf = 2, names = NULL, consensus = "consensus", mds_package = "ade4") {
  dist_mat <- compute_geodesic(tree_path)
  df <- data.frame(matrix(data = NA, nrow = nrow(dist_mat), ncol = nf + 2))
  if (is.null(names)) {
    df[, 1] <- paste0("tree", 1:nrow(dist_mat))
  } else {
    df[, 1] <- names
  }
  df[, 2] <- rep("gene", nrow(dist_mat))
  if (!is.null(consensus)) {
    df[1, 2] <- consensus
  }
  if (mds_package == "ade4") {
    pco <- ade4::dudi.pco(stats::as.dist(dist_mat), nf = nf, scannf = "FALSE")
    for (i in 1:nf) {
      df[, i + 2] <- pco$tab[, paste0("A", i)]
    }
  }
  if (mds_package == "smacof") {
    mds <- smacof::mds(stats::as.dist(dist_mat), ndim = nf)
    for (i in 1:nf) {
      df[, i+2] <- mds$conf[,i]
    }
  }
  names(df) <- c("tree_name", "tree_type", paste0("MDS",1:nf))
  return(list("dist_mat" = dist_mat, "df" = df))
}

