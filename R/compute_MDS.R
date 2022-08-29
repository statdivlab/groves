#' MDS on trees
#' Calculates the distance between each tree and performs MDS
#'
#' @param trees_path The file path to a txt file containing the set of phylogenetic trees in a 
#' multiPhylo object, required unless a dist_matrix is not NULL. 
#' @param nf The number of axes in the MDS to keep. Defaults to 2. 
#' @param dist_metric Either 'RF' for Robinson-Foulds, or 'BHV' for the geodesic defined in 
#' Billera et al. (2001). Defaults to 'BHV'. Ignore this argument if using argument dist_matrix.
#' @param dist_matrix If you would like to submit your own distance matrix computed with a different
#' metric, use this argument. In this case, do not include the dist_metric argument. 
#' @param mds_type MDS is performed with the package smacof. Default is metric scaling, otherwise
#' use value "nonmetric". 
#' @param tree_names Optional tree names to use.
#'
#' @return A list containing an object 'dist_mat' containing distances between each phylogenetic tree
#' in the multiPhylo object, and an object 'df' containing the desired number of axes in the MDS of the trees.
#' 
#' @examples 
#' trees_path <- system.file("txt", "small_tree_set.txt", package = "groves")
#' compute_MDS(trees_path = trees_path, tree_names = paste0("tree", 1:3))
#' 
#' @export
compute_MDS <- function(trees_path = NULL, nf = 2, dist_metric = "BHV",
                         dist_matrix = NULL, mds_type = "metric", 
                         tree_names = NULL) {
  # check that if a metric has been input, it is either 'BHV' or 'RF'
  if (!(dist_metric %in% c("BHV", "RF"))) {
    stop("Please input either 'BHV' or 'RF' for dist_metric, or input your own distance matrix
         to the argument dist_matrix and ignore the argument dist_metric.")
  }
  # check that if mds type is either metric or nonmetric, otherwise throw error
  if (!(mds_type %in% c("metric", "nonmetric"))) {
    stop("Please input either 'metric' or 'nonmetric' for mds_type.")
  }
  # save distance matrix 
  if (!is.null(dist_matrix)) {
    dist_mat <- dist_matrix
    # check that matrix is square, symmetric, diagonals are 0, off diagonals are 
    # greater or equal to zero
    if (length(unique(dim(dist_matrix))) > 1) {
      stop("dist_matrix provided is not square.")
    } else if (sum(!(dist_matrix == t(dist_matrix))) != 0) {
      stop("dist_matrix is not symmetric.")
    } else if (sum(diag(dist_matrix)) != 0) {
      stop("dist_matrix does not have 0 for each diagonal entry.")
    } else if (sum(dist_matrix < 0) != 0) {
      stop("dist_matrix is not non-negative.")
    }
  } else {
    # check that tree paths have been submitted
    if (is.null(trees_path)) {
      stop("Please submit either tree_paths or dist_matrix to compute distances between trees.")
    }
    if (dist_metric == "BHV") {
      dist_mat = compute_geodesic(trees_path)
    } else {
      trees <- ape::read.tree(trees_path)
      dist_mat <- as.matrix(phangorn::RF.dist(trees))
    }
  }
  # make dataframe to save MDS coordinates 
  df <- data.frame(matrix(data = NA, nrow = nrow(dist_mat), ncol = nf))
  if (is.null(tree_names)) {
    row.names(dist_mat) <- paste0("tree", 1:nrow(dist_mat))
    row.names(df) <- paste0("tree", 1:nrow(dist_mat))
  } else {
    row.names(dist_mat) <- tree_names
    row.names(df) <- tree_names
  }
  if (mds_type == "metric") {
    # run metric MDS
    mds <- smacof::mds(stats::as.dist(dist_mat), ndim = nf, type = "ratio")
    for (i in 1:nf) {
      df[, i] <- mds$conf[,i]
    }
  } else {
    # run non-metric MDS
    mds <- smacof::mds(stats::as.dist(dist_mat), ndim = nf, type = "ordinal")
    for (i in 1:nf) {
      df[, i] <- mds$conf[,i]
    }
  }
  names(df) <- paste0("MDS",1:nf)
  return(list("dist_mat" = dist_mat, "df" = df))
}

