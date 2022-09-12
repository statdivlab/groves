#' Add a vector
#' Computes the log map vector for a new tree from a base tree and adds it to an existing
#' set of log map vectors. 
#'
#' @param new_tree_path The path to the new tree to add.
#' @param base_tree_path Path to the base tree.
#' @param vectors Set of log map vectors for original set of trees.
#' @param new_name Optional name of new tree to be added.
#' @param tree_names Optional vector of names of original set of trees.
#' 
#' @return A set of log map vectors with a new vector added for the new tree.
#' 
#' @examples 
#' path <- paste0(system.file("txt", package = "groves"), "/")
#' tree_names <- c("tree1", "tree2", "tree3")
#' tree_paths <- paste0(path, "tree", 1:3, ".txt")
#' lm_res <- compute_logmap(tree_paths = tree_paths, tree_names = tree_names)
#' new <- paste0(path, "tree4.txt")
#' base <- tree_paths[which(lm_res$base_lab %in% tree_names)]
#' add_vector(new_tree_path = new, base_path = base, vectors = lm_res$vectors,
#'            new_name = "tree4", tree_names = tree_names)
#'
#' @export
add_vector <- function(new_tree_path, base_path, vectors, 
                       new_name = NULL, tree_names = NULL) {
  # path to the java file that will compute logmap
  lm_path <- system.file("java", "logmap.jar", package = "groves")
  
  # make new matrix for vectors with extra row 
  n <- nrow(vectors)
  new_vectors <- matrix(nrow = n + 1, ncol = ncol(vectors))
  new_vectors[1:n, ] <- vectors
  
  # get log map vector for new tree 
  res <- system2('java',
                 args = c('-jar', lm_path,
                          base_path,
                          tree_paths[1]),
                 stdout = T)
  eval(parse(text=res[length(res)]))
  new_vectors[n + 1, 1:length(logMap)] <- logMap
  
  # add new name to tree names 
  if (!is.null(tree_names)) {
    if (!is.null(new_name)) {
      tree_names <- c(tree_names, new_name)
    } else {
      tree_names <- c(tree_names, "new tree")
    }
  }
  
  # return new vectors and new names
  return(list(new_vectors = new_vectors, new_names = tree_names))
}

