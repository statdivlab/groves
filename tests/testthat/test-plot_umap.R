test_that("plot_umap works without phylogenomic or group", {
  trees_path <- paste0(system.file("txt", package = "groves"), "/")
  lm_vectors <- compute_logmap(tree_paths = paste0(trees_path, "tree", 1:4, ".txt"),
                               tree_names = c("tree1", "tree2", "tree3", "tree4"))$vectors
  names <- paste0("tree", 1:4)
  med_branch <- c(1, 5, 2, 3)
  expect_type(plot_umap(vectors = lm_vectors), "list")
})

test_that("plot_umap works without phylogenomic with group", {
  trees_path <- paste0(system.file("txt", package = "groves"), "/")
  lm_vectors <- compute_logmap(tree_paths = paste0(trees_path, "tree", 1:4, ".txt"),
                               tree_names = c("tree1", "tree2", "tree3", "tree4"))$vectors
  names <- paste0("tree", 1:4)
  med_branch <- c(1, 5, 2, 3)
  expect_type(plot_umap(vectors = lm_vectors, group = med_branch, tree_names = names), 
              "list")
})

test_that("plot_umap works with phylogenomic without group", {
  trees_path <- paste0(system.file("txt", package = "groves"), "/")
  lm_vectors <- compute_logmap(tree_paths = paste0(trees_path, "tree", 1:4, ".txt"),
                               tree_names = c("tree1", "tree2", "tree3", "tree4"))$vectors
  names <- paste0("tree", 1:4)
  med_branch <- c(1, 5, 2, 3)
  expect_type(plot_umap(vectors = lm_vectors, phylogenomic = 1, use_plotly = TRUE), 
              "list")
})

test_that("plot_umap works with phylogenomic and group", {
  trees_path <- paste0(system.file("txt", package = "groves"), "/")
  lm_vectors <- compute_logmap(tree_paths = paste0(trees_path, "tree", 1:4, ".txt"),
                               tree_names = c("tree1", "tree2", "tree3", "tree4"))$vectors
  names <- paste0("tree", 1:4)
  med_branch <- c(1, 5, 2, 3)
  tree_type <- c("ribosomal", "ribosomal", "other", "other")
  expect_type(plot_umap(vectors = lm_vectors, phylogenomic = 1, group = tree_type, 
                        tree_names = names, use_plotly = TRUE, show_legend = FALSE), 
              "list")
})

test_that("plot_umap works with phylogenomic and other tree", {
  trees_path <- paste0(system.file("txt", package = "groves"), "/")
  lm_vectors <- compute_logmap(tree_paths = paste0(trees_path, "tree", 1:4, ".txt"),
                               tree_names = c("tree1", "tree2", "tree3", "tree4"))$vectors
  names <- paste0("tree", 1:4)
  med_branch <- c(1, 5, 2, 3)
  tree_type <- c("ribosomal", "ribosomal", "other", "other")
  expect_type(plot_umap(vectors = lm_vectors, phylogenomic = 1, group = tree_type, 
                        tree_names = names, use_plotly = TRUE, show_legend = FALSE,
                        other_tree = 2), 
              "list")
})

test_that("plot_umap labels trees", {
  trees_path <- paste0(system.file("txt", package = "groves"), "/")
  lm_vectors <- compute_logmap(tree_paths = paste0(trees_path, "tree", 1:4, ".txt"),
                               tree_names = c("tree1", "tree2", "tree3", "tree4"))$vectors
  names <- paste0("tree", 1:4)
  med_branch <- c(1, 5, 2, 3)
  tree_type <- c("ribosomal", "ribosomal", "other", "other")
  expect_type(plot_umap(vectors = lm_vectors, phylogenomic = 1, group = tree_type, 
                        tree_names = names, use_plotly = TRUE, show_legend = FALSE,
                        trees_to_label = "tree3"), 
              "list")
})
