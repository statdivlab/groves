#' Lists genes without missingness
#' Takes a list of genes and returns the list with genes removed that are missing one or more tips.
#'
#' @param gene_names A list of gene names that correspond with alignments to examine
#' @param path The file path to the gene alignments, default is "
#' @param tail The extension to the gene alignments, default is ".fa
#' @param exclude_outgroup Binary variable, if true target genes will not need to be in the
#' outgroup genome.
#' @param outgroup Name of the outgroup, required if \code{exclude_outgroup = T}.
#'
#' @return A list containing a subset of the gene_names list only including names of genes without
#' missingness and the full presence matrix.
#'
#'
#' @export
get_target_genes <- function(gene_names, path = "", tail = ".fa", exclude_outgroup = FALSE,
                             outgroup = NULL) {

  # initializing the vector we'll fill as we find the good ones
  #target_genes <- vector()

  # get first gene alignment to get number of tips per file and tip names
  first_fasta <- paste0(path, gene_names[1], tail)
  #num_tips <- R.utils::countLines(first_fasta)[1]/2 # divide by two because each tip takes up 2 lines
  first_text <- readLines(first_fasta)
  num_tips <- length(first_text)/2
  tip_names_tmp <- first_text[seq(1, length(first_text), 2)]
  tip_names <- substr(tip_names_tmp, 2, nchar(tip_names_tmp))

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
