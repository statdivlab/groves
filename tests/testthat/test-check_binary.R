test_that("check error triggers if no arguments given", {
  expect_error(check_binary(), 
               "Please input either a tree or path to a tree.")
})

test_that("check error triggers if tree is not a phylo or multiPhylo object", {
  expect_error(check_binary("tree"),
               "tree must be a phylo or multiPhylo object.")
})

test_that("check function works for a tree path", {
  path <- system.file("txt/tree1.txt", package = "groves")
  expect_true(check_binary(tree_path = path))
})

test_that("check function works for a single tree", {
  expect_true(check_binary(ape::rtree(5)))
})

test_that("check function works for a set of trees", {
  expect_equal(check_binary(ape::rmtree(5, 5)), rep(TRUE, 5))
})
