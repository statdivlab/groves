test_that("compute_logmap has a row for each tree", {
  path <- paste0(system.file("txt", package = "groves"), "/")
  tree_paths <- paste0(path, "tree", 1:3, ".txt")
  logmap <- compute_logmap(base_path = paste0(path, "tree1.txt"),
                tree_paths = tree_paths,
                base_in_tree_paths = TRUE,
                tree_names = c("tree1", "tree2", "tree3"))
  expect_equal(nrow(logmap), length(tree_paths))
})

test_that("compute_logmap has a row for each tree still", {
  path <- paste0(system.file("txt", package = "groves"), "/")
  tree_paths <- paste0(path, "tree", 2:3, ".txt")
  logmap <- compute_logmap(base_path = paste0(path, "tree1.txt"),
                           tree_paths = tree_paths,
                           base_in_tree_paths = FALSE,
                           tree_names = c("tree2", "tree3"))
  expect_equal(nrow(logmap), length(tree_paths) + 1)
})

test_that("compute_logmap has the right number of columns", {
  path <- paste0(system.file("txt", package = "groves"), "/")
  tree_paths <- paste0(path, "tree", 1:3, ".txt")
  base_path <- paste0(path, "tree1.txt")
  base_tree <- ape::read.tree(base_path)
  n_tips <- length(base_tree$tip.label)
  exp_col <- (n_tips - 3)*2 + 3
  logmap <- compute_logmap(base_path = base_path,
                           tree_paths = tree_paths,
                           base_in_tree_paths = TRUE,
                           tree_names = c("tree1", "tree2", "tree3"))
  expect_equal(ncol(logmap), exp_col)
})
