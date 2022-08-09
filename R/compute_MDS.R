#' MDS on trees
#' Calculates the distance between each tree and performs MDS
#'
#' @param trees_path The file path to a txt file containing the set of phylogenetic trees in a 
#' multiPhylo object.
#' @param nf The number of axes in the MDS to keep. Defaults to 2. 
#' @param dist_metric Either 'RF' for Robinson-Foulds, or 'BHV' for the geodesic defined in 
#' Billera et al. (2001). Defaults to 'BHV'. Ignore this argument if using argument dist_matrix.
#' @param dist_matrix If you would like to submit your own distance matrix computed with a different
#' metric, use this argument. In this case, do not include the dist_metric argument. 
#' @param mds_package The default is to run classical MDS using ade4. To change this to metric MDS using the
#' package smacof, use the argument mds_package = "smacof".
#' @param tree_names Optional tree names to use.
#'
#' @return A list containing an object 'dist_mat' containing distances between each phylogenetic tree
#' in the multiPhylo object, and an object 'df' containing the desired number of axes in the MDS of the trees.
#' 
#' @examples 
#' trees_path <- system.file("txt", "small_tree_set.txt", package = "groves")
#' compute_MDS(trees_path, tree_names = paste0("tree", 1:3))
#' 
#' 
#' @export
compute_MDS <- function(trees_path, nf = 2, dist_metric = "BHV",
                         dist_matrix = NULL, mds_package = "ade4", 
                         tree_names = NULL) {
  # check that if a metric has been input, it is either 'BHV' or 'RF'
  if (!(dist_metric %in% c("BHV", "RF"))) {
    stop("Please input either 'BHV' or 'RF' for dist_metric, or input your own distance matrix
         to the argument dist_matrix and ignore the argument dist_metric.")
  }
  # save distance matrix 
  if (!is.null(dist_matrix)) {
    dist_mat <- dist_matrix
  } else {
    if (dist_metric == "BHV") {
      dist_mat = compute_geodesic(trees_path)
    } else {
      trees <- ape::read.tree(trees_path)
      dist_mat <- as.matrix(phangorn::RF.dist(trees))
    }
  }
  # make dataframe to save MDS coordinates 
  df <- data.frame(matrix(data = NA, nrow = nrow(dist_mat), ncol = nf))
  if (is.null(names)) {
    row.names(dist_mat) <- paste0("tree", 1:nrow(dist_mat))
    row.names(df) <- paste0("tree", 1:nrow(dist_mat))
  } else {
    row.names(dist_mat) <- tree_names
    row.names(df) <- tree_names
  }
  # run classical MDS
  if (mds_package == "ade4") {
    pco <- ade4::dudi.pco(stats::as.dist(dist_mat), nf = nf, scannf = "FALSE")
    for (i in 1:nf) {
      df[, i] <- pco$tab[, paste0("A", i)]
    }
  }
  # run non-metric MDS
  if (mds_package == "smacof") {
    mds <- smacof::mds(stats::as.dist(dist_mat), ndim = nf)
    for (i in 1:nf) {
      df[, i] <- mds$conf[,i]
    }
  }
  names(df) <- paste0("MDS",1:nf)
  return(list("dist_mat" = dist_mat, "df" = df))
}

