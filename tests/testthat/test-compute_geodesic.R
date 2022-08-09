test_that("compute_geodesic self distances are 0", {
  dists <- compute_geodesic(system.file("txt", "small_tree_set.txt", package = "groves"))
  expect_equal(dists[1, 1], 0)
})

test_that("compute_geodesic distances are symmetric", {
  dists <- compute_geodesic(system.file("txt", "small_tree_set.txt", package = "groves"))
  expect_equal(dists[1, 2], dists[2, 1])
})
