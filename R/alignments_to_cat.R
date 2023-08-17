#' Concatenates alignments 
#' Concatenates individual alignments into concatenated alignment. Will
#' optionally create a new folder to store all individual alignments and
#' concatenated alignment. 
#'
#' @param target_genes A list of target gene names.
#' @param path_from Path to find target gene alignment files.
#' @param tail Tail of alignment file after gene name. 
#' @param path_to Optional path to new folder to save all alignments. Defaults
#' to "" which does not create a new folder. 
#' @param concat_name A name for the concatenated alignment. 
#' @param sep_string String to use to separate individual alignments within the 
#' concatenated alignment. 
#'
#' @return Nothing, creates a folder in the desired directory and a concatenated
#' alignment.
#'
#' @export
alignments_to_cat <- function(target_genes, path_from = "", 
                              tail = ".fa", path_to = "",
                              concat_name = "concatenated",
                              sep_string = "XXXXX") {
  
  # optionally create new folder 
  if (path_to != "") {
    # make new directory
    dir.create(path_to)
    
    # copy files into new directory 
    for (curr_gene in target_genes) {
      file.copy(from = paste0(path_from, curr_gene, tail),
                to = paste0(path_to))
    }
  }  
  
  # combine all files into one 
  files <- paste0(path_from, target_genes, tail)
  full_align <- lapply(files, readLines)
  combined_data <- do.call(paste, c(full_align, sep = sep_string))
  combined_data[seq(1, length(combined_data), 2)] <- 
    unlist(lapply(stringr::str_split(
      combined_data[seq(1, length(combined_data), 2)], sep_string), 
      function(x) x[1]))
  
  # write concatenated alignment file
  if (path_to == "") {
    writeLines(combined_data, paste0(path_from, concat_name, tail))
  } else {
    writeLines(combined_data, paste0(path_to, concat_name, tail))
  }
}
