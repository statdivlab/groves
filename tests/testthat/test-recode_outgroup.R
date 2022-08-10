test_that("recode_outgroup works for 1 tree", {
  path <- system.file("txt", "small_tree_set.txt", package = "groves")
  tree <- ape::read.tree(path)[[1]]
  value <- 15
  tree_lengths_value <- sum(tree$edge.length == value)
  new_tree <- recode_outgroup(tree = tree, outgroup = "t1", value = value)
  expect_equal(sum(new_tree$edge.length == value) - tree_lengths_value, 1)
})

test_that("recode_outgroup works for multiple trees", {
  path <- system.file("txt", "small_tree_set.txt", package = "groves")
  tree_set <- ape::read.tree(path)
  new_tree_set <- recode_outgroup(tree = tree_set, outgroup = "t1")
  expect_equal(length(new_tree_set), length(tree_set))
})

test_that("recode_outgroup throws error when needed", {
  path <- system.file("txt", "small_tree_set.txt", package = "groves")
  tree_set <- ape::read.tree(path)
  expect_error(recode_outgroup(tree = tree_set, outgroup = "t11"),
               "The provided outgroup does not exist in tree 1 of the set.")
})
