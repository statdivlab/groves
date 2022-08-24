test_that("get_presence works", {
  gene_set <- paste0("gene_", 1:3)
  faa_path <- paste0(system.file("faa/", package = "groves"), "/")
  mat <- get_presence(gene_names = gene_set, path = faa_path, tail = ".faa")
  expect_equal(as.vector(mat[, 1]), c(1, 0, 1))
})

test_that("get_presence adds neccessary columns", {
  gene_set <- paste0("gene_", 1:3)
  faa_path <- paste0(system.file("faa/", package = "groves"), "/")
  mat <- get_presence(gene_names = gene_set, path = faa_path, tail = ".faa", extra_col = 1)
  expect_equal(as.vector(mat[, 1]), c(1, 0, 1))
})

