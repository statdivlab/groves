#' Finds missing genes
#' Provides a subset of genes from a list that are missing in the outgroup.
#'
#' @param gene_names List of genes of interest
#' @param presence Matrix of presence data for genes and genomes (TRUE if row gene is
#' present in column genome).
#' @param outgroup Name of the outgroup genome.
#'
#' @return A subset of gene_names that are missing in the outgroup genome
#'
#'
#' @export
genes_missing_outgroup <- function(gene_names, presence, outgroup) {
  miss <- names(which(!presence[gene_names, outgroup]))
  return(miss)
}
