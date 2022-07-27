#' Removes empty lines
#' Removes empty lines from a fasta file (so it can be run with IQTREE).
#'
#' @param target_genes A list of gene names for alignment files to remove empty lines from.
#' @param path Path to the alignment files.
#' @param tail Ending of alignment files.
#'
#'
#' @export
remove_empty_lines <- function(target_genes, path = "", tail = ".fa") {
  for (i in 1:length(target_genes)) {
    fasta <- paste0(path, target_genes[i],tail)
    fasta_contents <- readLines(fasta)
    fasta_length <- nchar(fasta_contents)[2]
    missing_ind <- which(!(fasta_contents !=
                             paste0(rep("-", fasta_length), collapse = "")))
    if (length(missing_ind) > 0) {
      rm_ind <- c(missing_ind - 1, missing_ind)
      fasta_miss_rm <- fasta_contents[-rm_ind]
      fileConn <- file(fasta)
      writeLines(fasta_miss_rm, fileConn)
      close(fileConn)
    }
  }
}

