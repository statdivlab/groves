test_that("standardize_branches works for single tree", {
  path <- system.file("txt", "small_tree_set.txt", package = "groves")
  tree_set <- ape::read.tree(path)
  new_tree <- standardize_branches(tree_set[[1]])
  expect_equal(sum(new_tree$edge.length), 1)
})

test_that("standardize_branches works for single tree setting branch lengths to 1", {
  path <- system.file("txt", "small_tree_set.txt", package = "groves")
  tree_set <- ape::read.tree(path)
  new_tree <- standardize_branches(tree_set[[1]], "one")
  expect_equal(new_tree$edge.length[1], 1)
})

test_that("standardize_branches works for multiple trees", {
  path <- system.file("txt", "small_tree_set.txt", package = "groves")
  tree_set <- ape::read.tree(path)
  new_tree_set <- standardize_branches(tree_set, denom = "max_branch")
  expect_equal(max(new_tree_set[[1]]$edge.length), 1)
})

test_that("standardize_branches throws error for non-tree object", {
  expect_error(standardize_branches("tree_set"), 
               "Please submit a tree (phylo object) or set of trees (multiPhylo object).",
               fixed = TRUE)
})

test_that("standardize_branches throws error for tree with all branch lengths of 0", {
  path <- system.file("txt", "small_tree_set.txt", package = "groves")
  tree_set <- ape::read.tree(path)
  zero_tree <- tree_set[[1]]
  zero_tree$edge.length <- rep(0, length(zero_tree$edge.length))
  expect_error(standardize_branches(zero_tree), 
               "All branch lengths in tree are equal to 0. This tree cannot be standardized.")
})
