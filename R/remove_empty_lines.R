#' Removes empty lines
#' Removes empty lines from a fasta file (so it can be run with IQTREE).
#'
#' @param target_genes A list of gene names for alignment files to remove empty lines from.
#' @param path Path to the alignment files.
#' @param tail Ending of alignment files.
#' @param missing_char The character used in the alignment to denote a missing base. "-" by default.
#' @param new_file_suffix A suffix to amend to alignment files after empty lines are removed.
#' By default this is "", which means that the new file will replace the old file.
#' 
#' @examples 
#' gene_set <- c("BacA", "CorA")
#' faa_path <- paste0(system.file("faa/", package = "groves"), "/")
#' faa_tail <- "_aln_tips_miss.faa"
#' remove_empty_lines(target_genes = gene_set, path = faa_path, tail = faa_tail, 
#'                    new_file_suffix = "_clean")
#'
#' @export
remove_empty_lines <- function(target_genes, path = "", tail = ".fa", 
                               missing_char = "-", new_file_suffix = "") {
  # for each gene, open alignment file
  for (i in 1:length(target_genes)) {
    fasta <- paste0(path, target_genes[i], tail)
    fasta_contents <- readLines(fasta)
    fasta_length <- nchar(fasta_contents)[2]
    # look for lines that are made up fully of missing values, this represents
    # a tip with no genetic information for this gene
    missing_ind <- which(!(fasta_contents !=
                             paste0(rep(missing_char, fasta_length), collapse = "")))
    if (length(missing_ind) > 0) {
      # remove tips that are fully missing 
      rm_ind <- c(missing_ind - 1, missing_ind)
      fasta_miss_rm <- fasta_contents[-rm_ind]
      fileConn <- file(paste0(path, target_genes[i], new_file_suffix, tail))
      writeLines(fasta_miss_rm, fileConn)
      close(fileConn)
    }
  }
}

