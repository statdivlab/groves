test_that("check_support works", {
  path1 <- system.file("txt", "small_tree_set.txt", package = "groves")
  path2 <- system.file("txt", "tree1.txt", package = "groves")
  trees <- ape::read.tree(path1)
  main_tree <- ape::read.tree(path2)
  check_res <- check_gene_support(main_tree, trees, rooted = FALSE)
  expect_type(check_res, "double")
})

test_that("check_support works for rooted trees", {
  trees <- ape::rmtree(100, 5, rooted = FALSE)
  check_res <- check_gene_support(trees[[1]], trees[2:100], rooted = FALSE)
  expect_type(check_res, "double")
})

test_that("all supports are proportions", {
  path1 <- system.file("txt", "small_tree_set.txt", package = "groves")
  path2 <- system.file("txt", "tree1.txt", package = "groves")
  trees <- ape::read.tree(path1)
  main_tree <- ape::read.tree(path2)
  check_res <- check_gene_support(main_tree, trees, rooted = FALSE)
  expect_equal(sum(check_res <= 1 & check_res >= 0),
               length(check_res))
})
