test_that("rename_labs works for one tree", {
  path <- system.file("txt", "small_tree_set.txt", package = "groves")
  tree_set <- ape::read.tree(path)
  name_df <- data.frame(old = paste0("t", 1:10), new = paste0("tip", 1:10))
  new_tree_set <- rename_labs(match_df = name_df, tree = tree_set[[1]], old_label = "old", 
                              new_label = "new")
  expect_false(tree_set[[1]]$tip.label[1] %in% new_tree_set$tip.label)
})

test_that("rename_labs works for set of trees", {
  path <- system.file("txt", "small_tree_set.txt", package = "groves")
  tree_set <- ape::read.tree(path)
  name_df <- data.frame(old = paste0("t", 1:10), new = paste0("tip", 1:10))
  new_tree_set <- rename_labs(match_df = name_df, tree = tree_set, old_label = "old", 
                              new_label = "new")
  expect_false(tree_set[[1]]$tip.label[1] %in% new_tree_set[[1]]$tip.label)
})

test_that("rename_labs throws error input is not a tree object", {
  expect_error(rename_labs(match_df = name_df, tree = "tree_set", old_label = "old", 
                           new_label = "new"),
               "Please submit a tree (phylo object) or set of trees (multiPhylo object).",
               fixed = TRUE)
})
