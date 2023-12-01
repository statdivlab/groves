test_that("compute_MDS works with BHV distance and metric MDS", {
  path <- system.file("txt", "small_tree_set.txt", package = "groves")
  expect_type(compute_MDS(trees_path = path, tree_names = paste0("tree", 1:3)),
              "list")
})

test_that("compute_MDS works for RF distance and nonmetric MDS", {
  trees_path <- system.file("txt", "small_tree_set.txt", package = "groves")
  expect_type(compute_MDS(trees_path, dist_metric = "RF", mds_type = "nonmetric"),
              "list")
})

test_that("compute_MDS works for provided distance matrix", {
  expect_type(compute_MDS(dist_matrix = matrix(c(1,1,1,1,1,1,1,1,1), nrow = 3) - diag(3)),
              "list")
})

test_that("compute_MDS gives error for non-square distance matrix", {
  expect_error(compute_MDS(dist_matrix = matrix(c(1,1,1,1,1,1), nrow = 3)),
               "dist_matrix provided is not square.")
})

test_that("compute_MDS gives error for non-symmetric distance matrix", {
  expect_error(compute_MDS(dist_matrix = matrix(c(1,1,1,1,1,1,1,2,1), nrow = 3)),
               "dist_matrix is not symmetric.")
})

test_that("compute_MDS gives error for non-zero diagonal distance matrix", {
  expect_error(compute_MDS(dist_matrix = matrix(c(1,1,1,1,1,1,1,1,1), nrow = 3)),
               "dist_matrix does not have 0 for each diagonal entry.")
})

test_that("compute_MDS gives error for negative diagonal distance matrix", {
  expect_error(compute_MDS(dist_matrix = matrix(c(0,1,-1,1,0,1,-1,1,0), nrow = 3)),
               "dist_matrix is not non-negative.")
})

test_that("compute_MDS gives error with wrong metric", {
  trees_path <- system.file("txt", "small_tree_set.txt", package = "groves")
  expect_error(compute_MDS(trees_path, dist_metric = "wRF"),
              "Please input either 'BHV' or 'RF' for dist_metric, or input your own distance matrix
         to the argument dist_matrix and ignore the argument dist_metric.")
})

test_that("compute_MDS gives error with wrong mds_type", {
  trees_path <- system.file("txt", "small_tree_set.txt", package = "groves")
  expect_error(compute_MDS(trees_path, mds_type = "classical"),
               "Please input either 'metric' or 'nonmetric' for mds_type.")
})

test_that("compute_MDS gives error when trees_path and dist_matrix both null", {
  expect_error(compute_MDS(),
               "Please submit either tree_paths or dist_matrix to compute distances between trees.")
})
