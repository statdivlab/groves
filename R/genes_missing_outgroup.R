#' Finds missing genes
#' Provides a subset of genes from a list that are missing in the outgroup.
#'
#' @param presence Matrix of presence data for genes and genomes (an entry is TRUE if row gene is
#' present in column genome).
#' @param outgroup Name of the outgroup genome.
#'
#' @return A subset of gene_names that are missing in the outgroup genome
#'
#' @examples 
#' gene_set <- paste0("gene_", 1:3)
#' faa_path <- paste0(system.file("faa/", package = "groves"), "/")
#' pres <- get_presence(gene_names = gene_set, path = faa_path, tail = ".faa")
#' genes_missing_outgroup(pres, "tip_3")
#' 
#' @export
genes_missing_outgroup <- function(presence, outgroup) {
  # check that outgroup is included in presence matrix 
  if (!(outgroup %in% colnames(presence))) {
    stop("Outgroup not included in presence matrix.")
  }
  miss <- names(which(!presence[, outgroup]))
  return(miss)
}
