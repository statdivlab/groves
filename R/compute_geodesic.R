#' Geodesic implementation
#' Calculates the geodesic distance between each tree
#'
#' @param tree_path The file path to a txt file containing phylogenetic trees in a multiPhylo object.
#'
#' @return A matrix containing distances between each tree in the multiPhylo object. Self distances are set to 0.
#' @export
compute_geodesic <- function(tree_path) {
  output <- paste0("-o gtp_output.txt")
  code_path <- system.file("java", "gtp.jar", package = "TreeVizPackage")
  system2('java',
          args = c('-jar', code_path, "-d", output,
                   tree_path),
          stdout = T)
  geodesic <- utils::read.table("gtp_output.txt")
  system2("rm", args = "gtp_output.txt")
  n_pair <- 1+max(geodesic$V2)
  dist_mat <- matrix(data = 0, nrow = n_pair, ncol = n_pair)
  for (i in 1:nrow(geodesic)) {
    a <- geodesic$V1[i]
    b <- geodesic$V2[i]
    val <- geodesic$V3[i]
    dist_mat[a+1,b+1] <- val
    dist_mat[b+1,a+1] <- val
  }
  return(dist_mat)
}
