test_that("plot_logmap works without phylogenomic or group", {
  trees_path <- paste0(system.file("txt", package = "groves"), "/")
  lm_vectors <- compute_logmap(base_path = paste0(trees_path, "tree1.txt"),
                               tree_paths = paste0(trees_path, "tree", 1:3, ".txt"),
                               base_in_tree_paths = TRUE,
                               tree_names = c("tree1", "tree2", "tree3"))
  names <- paste0("tree", 1:3)
  med_branch <- c(1, 5, 2)
  expect_type(plot_logmap(vectors = lm_vectors), "list")
})

test_that("plot_logmap works without phylogenomic with group", {
  trees_path <- paste0(system.file("txt", package = "groves"), "/")
  lm_vectors <- compute_logmap(base_path = paste0(trees_path, "tree1.txt"),
                               tree_paths = paste0(trees_path, "tree", 1:3, ".txt"),
                               base_in_tree_paths = TRUE,
                               tree_names = c("tree1", "tree2", "tree3"))
  med_branch <- c(1, 5, 2)
  names <- paste0("tree", 1:3)
  expect_type(plot_logmap(vectors = lm_vectors, group = med_branch, tree_names = names), 
              "list")
})

test_that("plot_logmap works with phylogenomic without group", {
  trees_path <- paste0(system.file("txt", package = "groves"), "/")
  lm_vectors <- compute_logmap(base_path = paste0(trees_path, "tree1.txt"),
                               tree_paths = paste0(trees_path, "tree", 1:3, ".txt"),
                               base_in_tree_paths = TRUE,
                               tree_names = c("tree1", "tree2", "tree3"))
  med_branch <- c(1, 5, 2)
  names <- paste0("tree", 1:3)
  expect_type(plot_logmap(vectors = lm_vectors, phylogenomic = 1, use_plotly = TRUE), 
              "list")
})

test_that("plot_logmap works with phylogenomic and group", {
  trees_path <- paste0(system.file("txt", package = "groves"), "/")
  lm_vectors <- compute_logmap(base_path = paste0(trees_path, "tree1.txt"),
                               tree_paths = paste0(trees_path, "tree", 1:3, ".txt"),
                               base_in_tree_paths = TRUE,
                               tree_names = c("tree1", "tree2", "tree3"))
  med_branch <- c(1, 5, 2)
  names <- paste0("tree", 1:3)
  tree_type <- c("ribosomal", "ribosomal", "other")
  expect_type(plot_logmap(vectors = lm_vectors, phylogenomic = 1, group = tree_type, 
                          tree_names = names, use_plotly = TRUE, show_legend = FALSE), 
              "list")
})
