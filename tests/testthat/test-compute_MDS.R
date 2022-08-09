test_that("compute_MDS works", {
  trees_path <- system.file("txt", "small_tree_set.txt", package = "groves")
  expect_type(compute_MDS(trees_path, tree_names = paste0("tree", 1:3)), "list")
})

test_that("compute_MDS gives error with wrong metric", {
  trees_path <- system.file("txt", "small_tree_set.txt", package = "groves")
  expect_error(compute_MDS(trees_path, dist_metric = "wRF"), 
              "Please input either 'BHV' or 'RF' for dist_metric, or input your own distance matrix
         to the argument dist_matrix and ignore the argument dist_metric.")
})
