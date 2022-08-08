#' Check missingness
#' Compute the proportion of missingness for each genome and gene combination.
#'
#' @param gene_names A list of names of genes.
#' @param tip_names A list of names of genomes (tips of the phylogeny).
#' @param path The path to the fasta file.
#' @param tail The end of the path to the fasta file.
#'
#' @return A matrix with the proportion of missingness for each genome and gene combination.
#'
#' @export
check_missingness <- function(gene_names, tip_names, path = "", tail = ".fa") {
  miss_prop <- matrix(data = NA, nrow = length(gene_names), ncol = length(tip_names))
  rownames(miss_prop) <- gene_names
  colnames(miss_prop) <- tip_names

  command1 <- "while read i; do echo $i | wc -m; done <"
  command2 <- "while read i; do echo $i | grep -o '-'| wc -l; done <"
  
  indices <- (1:length(tip_names))*2
  for (i in 1:length(gene_names)) {
    filename <- paste0(path, gene_names[i], tail)
    full_command <- paste0(command1, filename)
    tot <- readr::parse_number(system(full_command, intern = T))
    full_command <- paste0(command2, filename)
    miss <- readr::parse_number(system(full_command, intern = T))
    prop <- miss/tot
    miss_prop[i, ] <- prop[indices]
  }
  return(miss_prop)
}
