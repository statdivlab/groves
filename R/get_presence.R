#' Makes presence/absence matrix 
#' Takes a list of genes and returns a presence/absence matrix for genomes that included
#' as tips in the alignment for each gene. 
#'
#' @param gene_names A list of gene names that correspond with alignments to examine
#' @param path The file path to the gene alignments, default is ""
#' @param tail The extension to the gene alignments, default is ".fa"
#' @param extra_col The number of extra columns added to presence matrix to account for 
#' tips that are in some genes but not others. Defaults to 10. 
#'
#' @return A matrix with rows for each gene and columns for each genome (tip). Entries 
#' equal to 1 mean the gene alignment includes that tip. 
#' 
#' @examples 
#' gene_set <- paste0("gene_", 1:3)
#' faa_path <- paste0(system.file("faa/", package = "groves"), "/")
#' get_presence(gene_names = gene_set, path = faa_path, tail = ".faa")
#'
#' @export
get_presence <- function(gene_names, path = "", tail = ".fa", extra_col = 10) {
  
  # get first gene alignment to get number of tips per file and tip names
  first_fasta <- paste0(path, gene_names[1], tail)
  first_text <- readLines(first_fasta)
  num_tips <- length(first_text)/2
  tip_names_full <- first_text[seq(1, length(first_text), 2)]
  tip_names <- substr(tip_names_full, 2, nchar(tip_names_full))
  
  # initialize matrix
  presence <- matrix(data = 0, nrow = length(gene_names), ncol = num_tips + extra_col)
  rownames(presence) <- gene_names
  colnames(presence) <- c(tip_names, rep("tmp", extra_col))
  curr_col <- num_tips + 1
  
  # iterating through genes
  for (i in 1:length(gene_names)) {
    
    # get current gene
    curr_gene <- gene_names[i]
    
    # setting path to alignment fasta
    curr_fasta <- paste0(path, curr_gene, tail)
    
    # read text 
    text <- readLines(curr_fasta)
    
    # get tips 
    tip_names_full <- text[seq(1, length(text), 2)]
    tip_names <- substr(tip_names_full, 2, nchar(tip_names_full))
    
    # check that tip names are included in presence matrix 
    new_tips <- which(!(tip_names %in% colnames(presence))) 
    nt_len <- length(new_tips)
    if (nt_len > 0) {
      # if new tips in alignment, add to presence matrix 
      for (nt in 1:nt_len) {
        colnames(presence)[curr_col] <- tip_names[new_tips[nt]]
        curr_col <- curr_col + 1
        # if no more additional columns to add, make matrix wider 
        if (curr_col > ncol(presence)) {
          new_pres <- matrix(0, nrow = nrow(presence), 
                             ncol = ncol(presence) + extra_col)
          new_pres[, 1:ncol(presence)] <- presence 
          rownames(new_pres) <- gene_names
          colnames(new_pres) <- c(colnames(presence), rep("tmp", extra_col))
          presence <- new_pres 
        }
      }
    }
    
    # getting length of current gene's alignment
    curr_alignment_length <- nchar(readLines(curr_fasta, n = 2)[2])
    
    # check if each line is filled with dashes
    gene_res <- 1*(text != 
                     paste0(rep("-", curr_alignment_length), 
                            collapse = ""))[seq(2, 2*length(tip_names), 2)]
    # fill in ith row of presence matrix 
    presence[i, match(tip_names, colnames(presence))] <- gene_res
  }
  
  # remove extra columns 
  col_to_rm <- which(colnames(presence) == "tmp")
  if (length(col_to_rm) > 0) {
    presence <- presence[, -col_to_rm]
  }
  
  return(presence = presence)
}
