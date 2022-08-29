test_that("plot_MDS works without phylogenomic or group", {
  trees_path <- system.file("txt", "small_tree_set.txt", package = "groves")
  plot_df <- compute_MDS(trees_path = trees_path, tree_names = paste0("tree", 1:3))$df
  expect_type(plot_MDS(df = plot_df), "list")
})

test_that("plot_MDS works without phylogenomic with group", {
  trees_path <- system.file("txt", "small_tree_set.txt", package = "groves")
  plot_df <- compute_MDS(trees_path = trees_path, tree_names = paste0("tree", 1:3))$df
  plot_df$med_branch <- c(1, 5, 2)
  expect_type(plot_MDS(df = plot_df, group = "med_branch", tree_names = plot_df$names), "list")
})

test_that("plot_MDS works with phylogenomic without group", {
  trees_path <- system.file("txt", "small_tree_set.txt", package = "groves")
  plot_df <- compute_MDS(trees_path = trees_path, tree_names = paste0("tree", 1:3))$df
  expect_type(plot_MDS(df = plot_df, phylogenomic = 1, use_plotly = TRUE), "list")
})

test_that("plot_MDS works with phylogenomic and group", {
  trees_path <- system.file("txt", "small_tree_set.txt", package = "groves")
  plot_df <- compute_MDS(trees_path = trees_path, tree_names = paste0("tree", 1:3))$df
  plot_df$type <- c("ribosomal", "ribosomal", "other")
  plot_df$names <- paste0("tree", 1:3)
  expect_type(plot_MDS(df = plot_df, phylogenomic = 1, group = plot_df$type,
                       use_plotly = TRUE, tree_names = "names", show_legend = FALSE),
              "list")
})
