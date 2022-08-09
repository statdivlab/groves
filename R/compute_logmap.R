#' Compute logmap
#' Compute the logmap centered at one tree for all other trees.
#'
#' @param base_path The file path to the tree to center the log map.
#' @param tree_paths A list of file paths to all other trees.
#' @param add_tip_branches A boolean. When true, the lengths of branches ending in tips will be added
#' to the log map coordinates. Default to TRUE. 
#' @param base_in_tree_paths A logical value, TRUE if the base tree is included in the tree
#' paths vector.
#' @param tree_names An optional vector of names of trees. This should be the same length and order
#' as tree_paths. If base_in_tree_paths = FALSE, this vector should not include the base tree name. 
#' If this is not given, trees in tree_paths will be named numerically based on their order. 
#'
#' @return A matrix with log map coordinates for all trees. Row names give the name of each tree, or
#' the order from tree_paths if tree_names are not provided. 
#' 
#' @examples 
#' path <- paste0(system.file("txt", package = "groves"), "/")
#' compute_logmap(base_path = paste0(path, "tree1.txt"),
#'                tree_paths = paste0(path, "tree", 1:3, ".txt"),
#'                base_in_tree_paths = TRUE,
#'                tree_names = c("tree1", "tree2", "tree3"))
#'
#' @export
compute_logmap <- function(base_path, tree_paths, add_tip_branches = TRUE, 
                           base_in_tree_paths = FALSE, tree_names = NULL) {
  # path the java file that will compute logmap
  jar_path <- system.file("java", "logmap.jar", package = "groves")

  # the number of trees in tree_paths 
  n <- length(tree_paths) 
  # logmap vector for base tree 
  res <- system2('java',
                 args = c('-jar', jar_path,
                          base_path,
                          base_path),
                 stdout = T)
  eval(parse(text=res[length(res)]))
  
  # make matrix to hold logmap vector results 
  logMap_vects <- matrix(nrow = (n+1), ncol = length(logMap))
  
  # if base tree isn't included in tree_paths, saved log map for base tree
  # as first entry 
  if (!base_in_tree_paths) {
    logMap_vects[1,] <- logMap
  }

  # compute log map vector for all trees included in tree_paths 
  for (i in 2:(n+1)) {
    res <- system2('java',
                   args = c('-jar', jar_path,
                            base_path,
                            tree_paths[i-1]),
                   stdout = T)
    eval(parse(text=res[length(res)]))
    logMap_vects[i,] <- logMap
  }
  # add on lengths for branches ending in tips
  if (add_tip_branches) {
    # read in base tree
    base_tree <- ape::read.tree(base_path)
    # add columns to hold lengths of branches to tips
    lm_with_pend <- matrix(data = NA, nrow = nrow(logMap_vects),
                           ncol = ncol(logMap_vects) + length(base_tree$tip.label))
    # save log map vectors into new matrix
    lm_with_pend[1:nrow(logMap_vects), 1:ncol(logMap_vects)] <- logMap_vects
    # label branches that lead to tips as pendants 
    # tips are encoded as numbers 1 through the number of tips, so branches leading to tips are
    # encoded as edges that end in numbers less than the number of tips
    pendant <- which(base_tree$edge[,2] <= length(base_tree$tip.label))
    base_tip_labs <- base_tree$tip.label
    pendant_lengths <- base_tree$edge.length[pendant]
    # save pendant lengths into new matrix 
    lm_with_pend[1, (ncol(logMap_vects)+1):ncol(lm_with_pend)] <- pendant_lengths
    # loop through trees in tree_paths 
    for (i in 1:length(tree_paths)) {
      tree <- ape::read.tree(tree_paths[i])
      pendant <- which(tree$edge[,2] <= length(tree$tip.label))
      pendant_lengths <- tree$edge.length[pendant]
      # reorder pendant lengths if tips are in a different order than tips in base tree 
      order <- match(base_tip_labs, tree$tip.label)
      lm_with_pend[(i+1), (ncol(logMap_vects)+1):ncol(lm_with_pend)] <- pendant_lengths[order]
    }
    logMap_vects <- lm_with_pend
  }
  if (is.null(tree_names)) {
    tree_names <- 1:length(tree_paths)
  }
  rownames(logMap_vects) <- c("base_tree", tree_names)
  if (base_in_tree_paths) {
    logMap_vects <- logMap_vects[2:nrow(logMap_vects),]
  }
  return(logMap_vects)
}
