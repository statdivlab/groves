test_that("check_support works", {
  path1 <- system.file("txt", "gene_trees.txt", package = "groves")
  path2 <- system.file("txt", "concat_tree.txt", package = "groves")
  gene_trees <- ape::read.tree(path1)
  concat_tree <- ape::read.tree(path2)
  check_res <- check_support(concat_tree, gene_trees)
  expect_type(check_res, "list")
})

test_that("average support is a proportion", {
  path1 <- system.file("txt", "gene_trees.txt", package = "groves")
  path2 <- system.file("txt", "concat_tree.txt", package = "groves")
  gene_trees <- ape::read.tree(path1)
  concat_tree <- ape::read.tree(path2)
  check_res <- check_support(concat_tree, gene_trees)
  expect_true(check_res$support_prop <= 1 & check_res$support_prop >= 0)
})

test_that("all supports are proportions", {
  path1 <- system.file("txt", "gene_trees.txt", package = "groves")
  path2 <- system.file("txt", "concat_tree.txt", package = "groves")
  gene_trees <- ape::read.tree(path1)
  concat_tree <- ape::read.tree(path2)
  check_res <- check_support(concat_tree, gene_trees)
  expect_equal(sum(check_res$branch_support <= 1 & check_res$branch_support >= 0),
               length(check_res$branch_support))
})
