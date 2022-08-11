test_that("rename_labs works", {
  path <- system.file("txt", "small_tree_set.txt", package = "groves")
  tree_set <- ape::read.tree(path)
  name_df <- data.frame(old = paste0("t", 1:10), new = paste0("tip", 1:10))
  new_tree_set <- rename_labs(match_df = name_df, tree = tree_set, old_label = "old", 
                              new_label = "new")
  expect_false(tree_set[[1]]$tip.label[1] %in% new_tree_set[[1]]$tip.label)
})
