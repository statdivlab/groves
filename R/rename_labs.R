#' Rename labels
#' Given a set of old and new labels, switch between the two.
#'
#' @param match_df A dataframe with matched columns for the old and new labels.
#' @param tree A tree to rename tip labels for.
#' @param old_label The name of the column in match_df containing the old tip labels.
#' @param new_label The name of the column in match_df containing the new tip labels.
#'
#' @return The tree with renamed tip labels.
#'
#' @export
rename_labs <- function(match_df, tree, old_label = "oldlab", new_label = "newlab") {
  tip_places <- base::match(tree$tip.label, unlist(match_df[old_label]))
  tree$tip.label <- as.character(match_df[tip_places,new_label])
  return(tree)
}
