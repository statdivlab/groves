#' Rename labels
#' Given a set of old and new labels, switch between the two.
#'
#' @param match_df A dataframe with matched columns for the old and new labels.
#' @param tree A tree or set of trees to rename tip labels for.
#' @param old_label The name of the column in match_df containing the old tip labels.
#' @param new_label The name of the column in match_df containing the new tip labels.
#'
#' @return The tree with renamed tip labels.
#' 
#' @examples 
#' path <- system.file("txt", "small_tree_set.txt", package = "groves")
#' tree_set <- ape::read.tree(path)
#' name_df <- data.frame(old = paste0("t", 1:10), new = paste0("tip", 1:10))
#' new_tree_set <- rename_labs(match_df = name_df, tree = tree_set, old_label = "old", 
#'                            new_label = "new")
#' 
#' @export
rename_labs <- function(match_df, tree, old_label = "oldlab", new_label = "newlab") {
  if (class(tree) == "phylo") {
    tip_places <- base::match(tree$tip.label, unlist(match_df[old_label]))
    tree$tip.label <- as.character(match_df[tip_places,new_label])
  } else if (class(tree) == "multiPhylo") {
    for (i in 1:length(tree)) {
      tip_places <- base::match(tree[[i]]$tip.label, unlist(match_df[old_label]))
      tree[[i]]$tip.label <- as.character(match_df[tip_places,new_label])
    }
  } else {
    stop("Please submit a tree (phylo object) or set of trees (multiPhylo object).")
  }
  return(tree)
}
