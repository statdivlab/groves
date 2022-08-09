#' Check missingness
#' Compute the proportion of missingness for each genome and gene combination.
#'
#' @param gene_names A list of names of genes.
#' @param tip_names A list of names of genomes (tips of the phylogeny).
#' @param path The path to the fasta file.
#' @param tail The end of the path to the fasta file.
#' @param missing_char The character used in the alignment to denote a missing base. "-" by default.
#'
#' @return A matrix with the proportion of missingness for each genome and gene combination.
#'
#' @examples 
#' genes <- c("BacA", "CorA")
#' tips <- c("GCA_000025925.1", "GCA_000144405.1")
#' path <- paste0(system.file("faa/", package = "groves"), "/")
#' miss <- check_missingness(gene_names = genes,
#'                          tip_names = tips, 
#'                          path = path, 
#'                          tail = "_aln.faa")
#'
#' @export
check_missingness <- function(gene_names, tip_names, 
                              path = "", tail = ".fa", 
                              missing_char = "-") {
  
  # make matrix of genes and genomes to check 
  miss_prop <- matrix(data = NA, nrow = length(gene_names), ncol = length(tip_names))
  rownames(miss_prop) <- gene_names
  colnames(miss_prop) <- tip_names

  # get total number of characters on each line
  command1 <- "while read i; do echo $i | wc -m; done <"
  # get total number of missing characters on each line 
  command2 <- paste0("while read i; do echo $i | grep -o '", missing_char, 
                     "'| wc -l; done <")
  
  indices <- (1:length(tip_names))*2
  # loop over genes
  for (i in 1:length(gene_names)) {
    filename <- paste0(path, gene_names[i], tail)
    if (!file.exists(filename)) {
      stop(paste0("The gene alignment for ", 
                  gene_names[i], 
                  " doesn't exist in the given directory."))
    }
    # get tips from gene alignment 
    tips <- gsub(">", "", system(paste0("awk 'NR % 2 == 1' ", filename), intern = T))
    # match desired tip names to tips in alignment
    indices <- rep(NA, length(tip_names))
    for (j in 1:length(indices)) {
      ind <- which(tip_names[j] == tips)
      if (length(ind) > 0) {
        indices[j] <- ind*2
      } 
    }
    
    full_command <- paste0(command1, filename)
    # get total number of characters in each line
    tot <- readr::parse_number(system(full_command, intern = T))
    full_command <- paste0(command2, filename)
    # get total number of missing characters in each line
    miss <- readr::parse_number(system(full_command, intern = T))
    # get proportion of missingness
    prop <- miss/tot
    miss_prop[i, ] <- prop[indices]
  }
  return(miss_prop)
}
