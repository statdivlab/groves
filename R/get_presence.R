#' Makes presence/absence matrix 
#' Takes a list of genes and returns a presence/absence matrix for genomes that included
#' as tips in the alignment for each gene. 
#'
#' @param gene_names A list of gene names that correspond with alignments to examine
#' @param path The file path to the gene alignments, default is ""
#' @param tail The extension to the gene alignments, default is ".fa"
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
get_presence <- function(gene_names = gene_set, path = "", tail = ".fa") {
  
  # get first gene alignment to get number of tips per file and tip names
  first_fasta <- paste0(path, gene_names[1], tail)
  first_text <- readLines(first_fasta)
  num_tips <- length(first_text)/2
  tip_names_full <- first_text[seq(1, length(first_text), 2)]
  tip_names <- substr(tip_names_full, 2, nchar(tip_names_tmp))
  
  # if exclude_outgroup = TRUE, make sure outgroup isn't NULL and matches one of the tip names
  if (exclude_outgroup) {
    if (is.null(outgroup)) {
      stop("Please include outgroup name.")
    }
    outgroup_ind <- which(tip_names == outgroup)
    if (length(outgroup_ind) == 0) {
      stop("Please enter an outgroup that appears in your data.")
    }
  }
  
  # initialize matrix
  presence <- matrix(data = TRUE, nrow = length(gene_names), ncol = num_tips)
  rownames(presence) <- gene_names
  colnames(presence) <- tip_names
  
  # iterating through genes
  for ( i in 1:length(gene_names) ) {
    
    # get current gene
    curr_gene <- gene_names[i]
    
    # setting path to alignment fasta
    curr_fasta <- paste0(path, curr_gene, tail)
    
    # getting length of current gene's alignment
    curr_alignment_length <- nchar(readLines(curr_fasta, n = 2)[2])
    
    # # adding gene to target gene list if there are no entries that are all gaps
    # if ( ! any(readLines(curr_fasta) == paste0(rep("-", curr_alignment_length), collapse = "")) ) {
    #   target_genes <- c(target_genes, curr_gene)
    # }
    
    # check if each line is filled with dashes
    gene_res <- readLines(curr_fasta) != paste0(rep("-", curr_alignment_length), collapse = "")
    # take the even numbered lines (that contain alignment data)
    presence[i,] <- gene_res[seq(2, length(gene_res), 2)]
    
  }
  
  # get target genes from presence mat
  if (!exclude_outgroup) {
    target_genes <- gene_names[which(rowSums(!presence) == 0)]
  } else {
    presence_tmp <- presence[,-outgroup_ind]
    target_genes <- gene_names[which(rowSums(!presence_tmp) == 0)]
  }
  
  
  return(list(target = target_genes, presence = presence))
}
