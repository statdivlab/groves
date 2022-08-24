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

test_that("recode_outgroup throws error when outgroup is missing in tree", {
  path <- system.file("txt", "small_tree_set.txt", package = "groves")
  tree_set <- ape::read.tree(path)
  expect_error(recode_outgroup(tree = tree_set[[1]], outgroup = "t11"),
               "Please provide outgroup included on input tree.")
})

test_that("recode_outgroup throws error when outgroup missing in first of set of trees", {
  path <- system.file("txt", "small_tree_set.txt", package = "groves")
  tree_set <- ape::read.tree(path)
  expect_error(recode_outgroup(tree = tree_set, outgroup = "t11"),
               "The provided outgroup does not exist in tree 1 of the set.")
})

test_that("recode_outgroup throws error when outgroup missing in middle of set of trees", {
  path <- system.file("txt", "small_tree_set.txt", package = "groves")
  tree_set <- ape::read.tree(path)
  tree_set[[2]] <- ape::drop.tip(tree_set[[2]], "t10")
  expect_error(recode_outgroup(tree = tree_set, outgroup = "t10"),
               "The provided outgroup does not exist in tree 2 of the set.")
})

test_that("recode_outgroup throws error input is not a tree object", {
  expect_error(recode_outgroup(tree = "tree_set", outgroup = "t10"),
               "Please submit a tree (phylo object) or set of trees (multiPhylo object).",
               fixed = TRUE)
})
