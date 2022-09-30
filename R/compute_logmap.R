#' Compute logmap
#' Compute the logmap centered at one tree for all other trees.
#'
#' @param tree_paths A list of file paths to all other trees.
#' @param add_tip_branches A boolean. When true, the lengths of branches ending in tips will be added
#' to the log map coordinates. Default to TRUE. 
#' @param tree_names An optional vector of names of trees. This should be the same length and order
#' as tree_paths. If base_in_tree_paths = FALSE, this vector should not include the base tree name. 
#' If this is not given, trees in tree_paths will be named numerically based on their order. 
#' @param base_lab An optional label to specify the base tree. If this argument is ignored, \code{groves}
#' will set the base tree to be the tree in the tree set with the smallest mean BHV distance from the
#' other trees. If included, base_lab can either be a name included in \code{tree_names} or a number 
#' corresponding with the desired tree's order in \code{tree_paths}.
#'
#' @return A matrix with log map coordinates for all trees. Row names give the name of each tree, or
#' the order from tree_paths if tree_names are not provided. 
#' 
#' @examples 
#' path <- paste0(system.file("txt", package = "groves"), "/")
#' compute_logmap(tree_paths = paste0(path, "tree", 1:3, ".txt"),
#'                tree_names = c("tree1", "tree2", "tree3"))
#'
#' @export
compute_logmap <- function(tree_paths, add_tip_branches = TRUE, 
                           tree_names = NULL, base_lab = NULL) {
  # path to the java file that will compute logmap
  lm_path <- system.file("java", "logmap.jar", package = "groves")
  
  # if tree names not given, give trees numbers 
  if (is.null(tree_names)) {
    tree_names <- 1:length(tree_paths)
  }
  
  # if base tree is not specified, find gene tree with minimum mean BHV distance 
  # from other trees 
  if (is.null(base_lab)) {
    # save contents in tree_paths txt files to a single txt file 
    txt_path <- "full_trees_file.txt"
    merge_txt(tree_paths, txt_path)
    # compute distances
    dists <- compute_geodesic(txt_path)
    # compute squared distances
    sq_dists <- dists^2
    # remove txt file with multiPhylo object
    unlink(txt_path)
    # find tree with lowest mean BHV distance from other trees
    diag(sq_dists) <- NA
    base_tree_number <- which.min(rowMeans(sq_dists, na.rm = TRUE))
    base_lab <- tree_names[base_tree_number]
  # otherwise, make sure that base_tree is a number or a name given in base trees
  } else {
    if (is.numeric(base_lab)) {
      base_tree_number <- base_lab
      base_lab <- tree_names[base_tree_number]
    } else {
      if (!(base_lab %in% tree_names)) {
        stop("base_lab given does not appear in tree_names vector. Please let groves
             calculate a base tree for you, input a number corresponding your desired
             base tree's position in tree_paths, or a name that appears in tree_paths.")
      }
      base_tree_number <- which(base_lab == tree_names)
    }
  }
  # save base tree path
  base_path <- tree_paths[base_tree_number]
  
  # the number of trees in tree_paths 
  n <- length(tree_paths) 
  # logmap vector for first tree 
  res <- system2('java',
                 args = c('-jar', lm_path,
                          base_path,
                          tree_paths[1]),
                 stdout = T)
  try(eval(parse(text=res[length(res)])))
  
  # make matrix to hold logmap vector results 
  logMap_vects <- try(matrix(nrow = n, ncol = length(logMap)))
  if (inherits(logMap_vects, "try-error")) {
    stop(paste0("The chosen base tree, ", base_lab, ", is on the boundary of tree space and the log
                map cannot be computed using this tree. If you are manually choosing the
                base tree, please try another one. If this base tree was chosen automatically,
                you can try setting a base tree manually, but if you are unable to find a tree
                that does not return this error, this method may not work for your tree set."))
  }
  logMap_vects[1,] <- logMap
  
  # compute log map vector for all trees other included in tree_paths 
  for (i in 2:n) {
    res <- system2('java',
                   args = c('-jar', lm_path,
                            base_path,
                            tree_paths[i]),
                   stdout = T)
    eval(parse(text=res[length(res)]))
    logMap_vects[i, ] <- logMap
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
    pendant <- which(base_tree$edge[, 2] <= length(base_tree$tip.label))
    base_tip_labs <- base_tree$tip.label
    pendant_lengths <- base_tree$edge.length[pendant]
    # loop through trees in tree_paths 
    for (i in 1:length(tree_paths)) {
      tree <- ape::read.tree(tree_paths[i])
      pendant <- which(tree$edge[, 2] <= length(tree$tip.label))
      pendant_lengths <- tree$edge.length[pendant]
      # reorder pendant lengths if tips are in a different order than tips in base tree 
      order <- match(base_tip_labs, tree$tip.label)
      lm_with_pend[i, (ncol(logMap_vects)+1):ncol(lm_with_pend)] <- pendant_lengths[order]
    }
    logMap_vects <- lm_with_pend
  }
  rownames(logMap_vects) <- tree_names
  return(list(vectors = logMap_vects, base_lab = base_lab))
}
