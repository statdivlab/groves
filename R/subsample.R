#' Subsamples genes and genomes 
#' Subsamples genes and genomes to come up with a subset of genes and genomes such that
#' every genome contains every gene. 
#'
#' @param gene_names A list of gene names that correspond with alignments to examine
#' @param path The file path to the gene alignments, default is ""
#' @param tail The extension to the gene alignments, default is ".fa"
#' @param genes_to_keep An optional list of genes to keep in the subsample
#' @param genomes_to_keep An optional list of genomes to keep in the subsample
#' @param gene_weight Set to 1, the number of genes to add at once. If gene_weight is
#' larger than genome_weight, more genes will be retained in the subsample than genomes
#' @param genome_weight Set to 1, the number of genomes to add at once. If 
#' genome_weight is larger than gene_weight, more genomes will be retained in the 
#' subsample than genes
#'
#' @return A matrix with rows for each gene and columns for each genome (tip). Entries 
#' equal to 1 mean the gene alignment includes that tip. 
#' 
#' @examples 
#' gene_set <- paste0("gene_", 1:3)
#' faa_path <- paste0(system.file("faa/", package = "groves"), "/")
#' subsample(gene_names = gene_set, path = faa_path, tail = ".faa")
#'
#' @export
subsample <- function(gene_names, path = "", tail = ".fa",
                      genes_to_keep = NULL, genomes_to_keep = NULL,
                      gene_weight = 1, genome_weight = 1) {
  # start by getting presence matrix of genes and genomes 
  presence <- get_presence(gene_names, path, tail)
  
  # copy the presence matrix into a matrix that will change size 
  upd_presence <- presence 
  
  # indicator of whether a gene or genome should be added 
  up_next <- 1
  
  # list of genes to retain 
  if (is.null(genes_to_keep)) {
    gene_set <- c()
  } else {
    if (sum(genes_to_keep %in% rownames(presence)) < length(genes_to_keep)) {
      stop("Some genes in genes_to_keep do not appear in your presence matrix. 
           Check gene_names to see which genes appear in your data.")
    }
    gene_set <- genes_to_keep
    # check that any genomes match all genes_to_keep
    rows_to_keep <- rownames(presence) %in% genes_to_keep 
    gene_set_mat <- presence[rows_to_keep, ]
    if (length(gene_set) > 1) {
      if (max(colSums(gene_set_mat)) < length(genes_to_keep)) {
        stop("There are no genomes that share all genes provided as 
           genes_to_keep.")
      }
      # find which genomes don't include chosen genes 
      col_to_rm <- which(colSums(gene_set_mat) < length(genes_to_keep))
    } else {
      # find which genomes don't include chosen genes 
      col_to_rm <- which(gene_set_mat == 0)
    }
    up_next <- -1 
    # remove genomes that don't include chosen genes
    if (length(col_to_rm) > 0) {
      upd_presence <- upd_presence[, -col_to_rm]
    }
  }
  
  # list of genomes to retain 
  if (is.null(genomes_to_keep)) {
    genome_set <- c()
  } else {
    # check that all genomes are in presence matrix 
    if (sum(genomes_to_keep %in% colnames(presence)) < length(genomes_to_keep)) {
      stop("Some genomes in genomes_to_keep do not appear in your presence matrix. 
           You can run get_presence() to identify genomes that appear in your data.")
    }
    genome_set <- genomes_to_keep
    # check that any genes are in all genomes to keep 
    cols_to_keep <- colnames(presence) %in% genomes_to_keep
    genome_set_mat <- presence[, cols_to_keep]
    if (length(genomes_to_keep) > 1) {
      if (max(rowSums(genome_set_mat)) < length(genomes_to_keep)) {
        stop("There are no genes in all genomes provided as 
           genomes_to_keep.")
      }
      # find which genes aren't in chosen genomes
      row_to_rm <- which(rowSums(genome_set_mat) < length(genomes_to_keep))
    } else {
      # find which genes aren't in chosen genomes
      row_to_rm <- which(genome_set_mat == 0)
    }
    # remove genes not in chosen genomes
    if (length(row_to_rm) > 0) {
      upd_presence <- upd_presence[-row_to_rm, ]
    } 
  }
  
  # continue to add genes and genomes until a complete set is found
  while(sum(upd_presence == 0) > 0) {
    if (!is.matrix(upd_presence)) {
      if (length(gene_set) < 2) {
        stop("Only one gene selected. You can look manually for a complete set but
             this dataset may not be well-suited for gene tree exploration.")
      } else if (length(genome_set) < 2) {
        stop("Only one genome selected. You can look manually for a complete set but
             this dataset may not be well-suited for gene tree exploration.")
      } 
    } else {
      if (up_next == 1) {
        # add genes 
        for (i in 1:gene_weight) {
          # get indices of rows that haven't been added yet 
          rows_left <- !(rownames(upd_presence) %in% gene_set)
          # find gene included in the most number of genomes
          if (is.matrix(upd_presence[rows_left, ])) {
            gene_to_add <- names(which.max(rowMeans(upd_presence[rows_left, ]))[1])
          } else {
            gene_to_add <- rownames(upd_presence)[rows_left]
          }
          
          # save gene in gene set 
          gene_set <- c(gene_set, gene_to_add)
          # find which genomes don't include chosen gene 
          col_to_rm <- which(upd_presence[rownames(upd_presence) == gene_to_add, ] == 0)
          # remove genomes that don't include chosen gene
          if (length(col_to_rm) > 0) {
            upd_presence <- upd_presence[, -col_to_rm]
          }
        }
      } else {
        # add genomes 
        for (i in 1:genome_weight) {
          # get indices of columns that haven't been added yet 
          cols_left <- !(colnames(upd_presence) %in% genome_set)
          # find genome that contains the most genes 
          if (is.matrix(upd_presence[, cols_left])) {
            genome_to_add <- names(which.max(colMeans(upd_presence[, !(colnames(upd_presence) 
                                                              %in% genome_set)]))[1])
          } else {
            genome_to_add <- colnames(upd_presence)[cols_left]
          }
        }
        
        # save genome in genome set
        genome_set <- c(genome_set, genome_to_add)
        # find which genes aren't in chosen genome
        row_to_rm <- which(upd_presence[, colnames(upd_presence) == genome_to_add] == 0)
        # remove genes that aren't in chosen genome
        if (length(row_to_rm) > 0) {
          upd_presence <- upd_presence[-row_to_rm, ]
        }
      }
    }
    # switch from gene to genome or vice versa for next update 
    up_next <- up_next * -1 
  }
  
  if (!is.matrix(upd_presence)) {
    if (length(gene_set) < 2) {
      warning("Only one gene selected. You can look manually for a complete set but
             this dataset may not be well-suited for gene tree exploration.")
    } else if (length(genome_set) < 2) {
      warning("Only one genome selected. You can look manually for a complete set but
             this dataset may not be well-suited for gene tree exploration.")
    }
  }
  
  complete_set <- list(gene_set = rownames(upd_presence), 
                       genome_set = colnames(upd_presence))
  return(list(presence = presence, complete_set = complete_set))
}
