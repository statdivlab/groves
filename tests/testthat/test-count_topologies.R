test_that("correct count for Prevotella", {
  path <- system.file("txt", "gene_trees.txt", package = "groves")
  gene_trees <- ape::read.tree(path)
  expect_equal(count_topologies(gene_trees)$count, 63)
})

test_that("counts trees with same topology the same", {
  tree1 <- ape::rtree(20)
  tree2 <- tree1
  trees <- c(tree1, tree2)
  expect_equal(count_topologies(trees)$count, 1)
})