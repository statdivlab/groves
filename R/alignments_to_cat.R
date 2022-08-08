#' Makes new folder with alignments
#' Makes a new folder with alignments to concatenate into a single alignment.
#'
#' @param target_genes A list of target gene names.
#' @param path_from Set to "".
#' @param tail Set to ".fa"
#' @param path_to Set to "".
#'
#' @return Nothing, creates a folder in the desired directory.
#'
#' @export
alignments_to_cat <- function(target_genes, path_from = "", tail = ".fa", path_to = "") {
  # make new directory
  dir.create(path_to)
  # copy files in
  for (curr_gene in target_genes) {
    file.copy(from = paste0(path_from,curr_gene,tail),
              to = paste0(path_to))
  }
}
