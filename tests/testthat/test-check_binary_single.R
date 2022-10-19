test_that("check_binary_single works", {
  # returns true when tree is binary
  path <- system.file("txt/tree1.txt", package = "groves")
  tree <- ape::read.tree(path)
  expect_true(check_binary_single(tree))
  unrooted_tree <- ape::rtree(5, rooted = FALSE)
  expect_true(check_binary_single(unrooted_tree))
  # returns false when tree is not binary because number of internal 
  # nodes is wrong
  tree_sm_nnodes <- tree
  tree_sm_nnodes$Nnode <- 2
  expect_false(check_binary_single(tree_sm_nnodes))
  # returns false when tree is not binary because it has at least one 
  # internal branch with length 0 
  tree_internal_zero <- tree
  # first edge is internal, set to 0
  tree_internal_zero$edge.length[c(1, 2)] <- 0 
  expect_false(check_binary_single(tree_internal_zero))
  # returns true when tree has edge length 0 only for pendant edge
  tree_external_zero <- tree
  tree_external_zero$edge.length[2] <- 0
  expect_true(check_binary_single(tree_external_zero))
})
