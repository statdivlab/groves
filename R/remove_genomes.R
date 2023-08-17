#' Removes genomes
#' Removes genome lines from a fasta file.
#'
#' @param target_genes A list of gene names for alignment files to remove empty lines from.
#' @param target_genomes A list of genome names to remove from alignment files
#' @param path Path to the alignment files.
#' @param new_folder_path Path to an optional new folder to put alignments in. 
#' @param tail Ending of alignment files.
#' @param new_file_suffix A suffix to amend to alignment files after empty lines are removed.
#' By default this is "", which means that the new file will replace the old file.
#'
#' @examples 
#' gene_set <- c("BacA", "CorA")
#' genome_set <- c("GCA_000025925.1", "GCA_000144405.1", "fake_tip")
#' faa_path <- paste0(system.file("faa/", package = "groves"), "/")
#' faa_tail <- "_aln.faa"
#' remove_genomes(target_genes = gene_set, target_genomes = genome_set,
#'                path = faa_path, tail = faa_tail, new_file_suffix = "_fewer_tips")
#'
#' @export
remove_genomes <- function(target_genes, target_genomes, path = "", tail = ".fa",
                           new_file_suffix = "", new_folder_path = "") {
  genomes_remove <- paste0(">", target_genomes)
  for (i in 1:length(target_genes)) {
    fasta <- paste0(path, target_genes[i], tail)
    fasta_contents <- readLines(fasta)
    missing_ind <- which(fasta_contents %in% genomes_remove)
    if (length(missing_ind) > 0) {
      rm_ind <- c(missing_ind, missing_ind + 1)
      fasta_miss_rm <- fasta_contents[-rm_ind]
      if (new_folder_path == "") {
        fileConn <- file(paste0(path, target_genes[i], new_file_suffix, tail))
      } else {
        fileConn <- file(paste0(new_folder_path, target_genes[i], new_file_suffix, tail))
      }
      writeLines(fasta_miss_rm, fileConn)
      close(fileConn)
    }
  }
}
