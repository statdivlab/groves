test_that("counts trees with same topology the same", {
  tree1 <- ape::rtree(20)
  tree2 <- tree1
  trees <- c(tree1, tree2)
  expect_equal(count_topologies(trees)$count, 1)
})

test_that("count_topologies can add names", {
  tree1 <- ape::rtree(20)
  tree2 <- tree1
  trees <- c(tree1, tree2)
  expect_equal(count_topologies(trees, c("tree1", "tree2"))$count, 1)
})
