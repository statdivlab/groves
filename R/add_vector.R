#' Add a vector
#' Computes the log map vector for a new tree from a base tree and adds it to an existing
#' set of log map vectors. 
#'
#' @param new_tree_path The path to the new tree to add.
#' @param base_path Path to the base tree.
#' @param vectors Set of log map vectors for original set of trees.
#' @param add_tip_branches A boolean. When true, the lengths of branches ending in tips will be added
#' to the log map coordinates. Default to TRUE. 
#' @param new_name Optional name of new tree to be added.
#' 
#' @return A set of log map vectors with a new vector added for the new tree.
#' 
#' @examples 
#' path <- paste0(system.file("txt", package = "groves"), "/")
#' tree_names <- c("tree1", "tree2", "tree3")
#' tree_paths <- paste0(path, "tree", 1:3, ".txt")
#' lm_res <- compute_logmap(tree_paths = tree_paths, tree_names = tree_names)
#' new <- paste0(path, "tree4.txt")
#' base <- tree_paths[which(tree_names %in% lm_res$base_lab)]
#' add_vector(new_tree_path = new, base_path = base, vectors = lm_res$vectors,
#'            new_name = "tree4")
#'
#' @export
add_vector <- function(new_tree_path, base_path, vectors, 
                       add_tip_branches = TRUE, new_name = NULL) {
  
  # make new matrix for vectors with extra row 
  n <- nrow(vectors)
  new_vectors <- matrix(nrow = n + 1, ncol = ncol(vectors))
  new_vectors[1:n, ] <- vectors
  
  # get log map vector for new tree 
  new_vectors[n + 1, ] <- compute_logmap(c(base_path, new_tree_path), 
                                         add_tip_branches = add_tip_branches,
                                         base_lab = 1)$vectors[2, ]
  
  # add new name to tree names 
  if (rownames(vectors)[1] == "1") {
    tree_names <- 1:(n + 1)
  } else {
    if (!is.null(new_name)) {
      tree_names <- c(rownames(vectors), new_name)
    } else {
      tree_names <- c(rownames(vectors), "new tree")
    }
  }
  rownames(new_vectors) <- tree_names
  
  # return new vectors and new names
  return(new_vectors = new_vectors)
}

