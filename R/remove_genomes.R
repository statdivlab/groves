#' Removes genomes
#' Removes genome lines from a fasta file.
#'
#' @param target_genes A list of gene names for alignment files to remove empty lines from.
#' @param missing_genomes A list of genome names to remove from alignment files
#' @param path Path to the alignment files.
#' @param tail Ending of alignment files.
#'
#'
#' @export
remove_genomes <- function(target_genes, missing_genomes, path = "", tail = ".fa") {
  genomes_remove <- paste0(">", missing_genomes)
  for (i in 1:length(target_genes)) {
    fasta <- paste0(path, target_genes[i],tail)
    fasta_contents <- readLines(fasta)
    fasta_length <- nchar(fasta_contents)[2]
    missing_ind <- which(fasta_contents %in% genomes_remove)
    if (length(missing_ind) > 0) {
      rm_ind <- c(missing_ind, missing_ind + 1)
      fasta_miss_rm <- fasta_contents[-rm_ind]
      fileConn <- file(fasta)
      writeLines(fasta_miss_rm, fileConn)
      close(fileConn)
    }
  }
}
