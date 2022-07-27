#' Compute logmap
#' Compute the logmap centered at one tree for all other trees.
#'
#' @param cons_path The file path to the tree to center the log map.
#' @param tree_paths A list of file paths to all other trees.
#' @param jar_path A optional file path to the jar file to compute the log map. Otherwise,
#' the logmap.jar file in the TreeVizPackage repo will be used.
#' @param other_cons_path An optional additional consensus tree to plot.
#' @param add_pendant_branches A boolean. When true, the pendant branch lengths will be added
#' to the log map coordinates.
#' @param cons_tree The consensus tree. This is required when \code{add_pendant_branches = TRUE}.
#' @param trees_complete Complete list of trees in a multiPhylo object including the base tree.
#'This is required when \code{add_pendant_branches = TRUE}.
#' @param cons_in_tree_paths A logical value, TRUE if the consensus tree is included in the tree
#' paths vector.
#'
#' @return A matrix with log map coordinates for all trees.
#'
#' @export
compute_logmap <- function(cons_path, tree_paths, jar_path = NULL, other_cons_path = NULL,
                           add_pendant_branches = FALSE, cons_tree = NULL,
                           trees_complete = NULL, cons_in_tree_paths = FALSE) {
  if (is.null(jar_path)) {
    jar_path <- system.file("java", "logmap.jar", package = "TreeVizPackage")
  }

  n <- length(tree_paths) # number of trees
  res <- system2('java',
                 args = c('-jar', jar_path,
                          cons_path,
                          cons_path),
                 stdout = T)
  eval(parse(text=res[length(res)]))
  logMap_dists <- matrix(nrow = (n+1), ncol = length(logMap))
  if (!is.null(other_cons_path)) {logMap_dists <- matrix(nrow = n+2, ncol = length(logMap))}

  if (!cons_in_tree_paths) {
    logMap_dists[1,] <- logMap
  }

  for (i in 2:(n+1)) {
    res <- system2('java',
                   args = c('-jar', jar_path,
                            cons_path,
                            tree_paths[i-1]),
                   stdout = T)
    eval(parse(text=res[length(res)]))
    logMap_dists[i,] <- logMap
  }
  if (!is.null(other_cons_path)) {
    res <- system2('java',
                   args = c('-jar', jar_path,
                            cons_path,
                            other_cons_path),
                   stdout = T)
    eval(parse(text=res[length(res)]))
    logMap_dists[dim(logMap_dists)[1],] <- logMap
  }
  if (add_pendant_branches) {
    if (is.null(cons_tree)) {
      stop("Please input consensus tree.")
    }
    if (cons_in_tree_paths) {
      lm_with_pend <- matrix(data = NA, nrow = nrow(logMap_dists),
                             ncol = ncol(logMap_dists) + length(cons_tree$tip.label))
      lm_with_pend[1:nrow(logMap_dists), 1:ncol(logMap_dists)] <- logMap_dists
      pendant <- which(cons_tree$edge[,2] <= length(cons_tree$tip.label))
      cat_tip_labs <- cons_tree$tip.label
      for (i in 1:length(trees_complete)) {
        tree <- trees_complete[[i]]
        pendant <- which(tree$edge[,2] <= length(tree$tip.label))
        pendant_lengths <- tree$edge.length[pendant]
        order <- match(cat_tip_labs, tree$tip.label)
        lm_with_pend[(i+1), (ncol(logMap_dists)+1):ncol(lm_with_pend)] <- pendant_lengths[order]
      }
      logMap_dists <- lm_with_pend
    } else {
      lm_with_pend <- matrix(data = NA, nrow = nrow(logMap_dists),
                             ncol = ncol(logMap_dists) + length(cons_tree$tip.label))
      lm_with_pend[1:nrow(logMap_dists), 1:ncol(logMap_dists)] <- logMap_dists
      pendant <- which(cons_tree$edge[,2] <= length(cons_tree$tip.label))
      lm_with_pend[1, (ncol(logMap_dists)+1):ncol(lm_with_pend)] <- cons_tree$edge.length[pendant]
      cat_tip_labs <- cons_tree$tip.label
      for (i in 1:(length(trees_complete)-1)) {
        tree <- trees_complete[[i+1]]
        pendant <- which(tree$edge[,2] <= length(tree$tip.label))
        pendant_lengths <- tree$edge.length[pendant]
        order <- match(cat_tip_labs, tree$tip.label)
        lm_with_pend[(i+1), (ncol(logMap_dists)+1):ncol(lm_with_pend)] <- pendant_lengths[order]
      }
      logMap_dists <- lm_with_pend
    }
  }
  if (cons_in_tree_paths) {
    logMap_dists <- logMap_dists[2:nrow(logMap_dists),]
  }
  return(logMap_dists)
}
