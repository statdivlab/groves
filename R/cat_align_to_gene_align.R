#' Creates gene alignments
#' Creates individual gene alignments from a concatenated alignment. 
#'
#' @param concat_path Path to concatenated alignment. 
#' @param gene_names A vector of gene names, ordered in the same way in which 
#' genes are concatenated in the concatenated alignment. If not provided, genes 
#' will be given numbers. 
#' @param sep_string The string that separates each gene alignment within the 
#' concatenated alignment. Defaults to "XXXXX". 
#' @param output_path The path to a folder in which the gene alignments will be saved. 
#' Make sure to add a "/" at the end of the path. 
#' @param tail The end of the gene alignment name. Defaults to ".faa".
#'
#' @return Nothing, creates a folder in the desired directory that includes the 
#' individual gene alignments. 
#'
#' @export
cat_align_to_gene_align <- function(concat_path, gene_names = NULL,
                                    sep_string = "XXXXX", 
                                    output_path = "gene_alignments/",
                                    tail = ".faa") {
  # create output folder if it doesn't already exist
  if (!file.exists(output_path)) {
    dir.create(output_path)
  }
  # read in concatenated alignment
  cat_align <- readLines(concat_path)
  # lines that given tip names
  tip_ind <- seq(1, length(cat_align), 2)
  tips <- cat_align[tip_ind]
  # lines that give sequences
  sequences <- cat_align[tip_ind + 1]
  # split sequences by separator string 
  sequence_list <- stringr::str_split(sequences, "XXXXX")
  # add names for genes if not given 
  if (is.null(gene_names)) {
    gene_names <- paste0("gene", 1:length(sequences_list[[1]]))
  }
  # loop over genes 
  for (gene_num in 1:length(gene_names)) {
    gene_align <- vector(mode = "character", length = 2*length(tips))
    gene_align[tip_ind] <- tips
    seq <- unlist(lapply(1:length(tips), function(x) {sequence_list[[x]][gene_num]}))
    gene_align[tip_ind + 1] <- seq
    writeLines(gene_align, paste0(output_path, gene_names[gene_num], ".faa"))
  }
}
