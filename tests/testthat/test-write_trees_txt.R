test_that("test write_trees_txt works for one tree", {
  tree <- ape::rtree(10)
  path <- system.file("txt", package = "groves")
  write_trees_txt(tree = tree, path = path)
  expect_true(file.exists(paste0(path, "/tree.txt")))
})

test_that("test write_trees_txt works for multiple trees", {
  trees <- ape::rmtree(2, 5)
  path <- system.file("txt", package = "groves")
  write_trees_txt(tree = trees, path = path, tree_names = paste0("test_trees", 1:2))
  expect_true(file.exists(paste0(path, "/test_trees1.txt")))
})

test_that("test write_trees_txt works for multiple trees when folder doesn't exist yet", {
  trees <- ape::rmtree(2, 5)
  path <- system.file("txt", package = "groves")
  write_trees_txt(tree = trees, path = paste0(path, "/test_trees"))
  expect_true(file.exists(paste0(path, "/test_trees/tree1.txt")))
})

test_that("test write_trees_txt throws error if no tree given", {
  expect_error(write_trees_txt(tree = NULL, path = "tree"),
               "Please submit a tree (phylo object) or set of trees (multiPhylo object).",
               fixed = TRUE)
})


