test_that("check_missingness works", {
  genes <- c("BacA", "CorA")
  tips <- c("GCA_000025925.1", "GCA_000144405.1")
  path <- paste0(system.file("faa/", package = "groves"), "/")
  miss <- check_missingness(gene_names = genes,
                           tip_names = tips, 
                           path = path, 
                           tail = "_aln.faa")
  expect_equal(sum(is.na(miss)), 0)
})

test_that("check_missingness throws gene error", {
  genes <- c("BacA", "CorA", "fake_gene")
  tips <- c("GCA_000025925.1", "GCA_000144405.1")
  path <- paste0(system.file("faa/", package = "groves"), "/")
  expect_error(check_missingness(gene_names = genes,
                                 tip_names = tips, 
                                 path = path, 
                                 tail = "_aln.faa"),
               "The gene alignment for fake_gene doesn't exist in the given directory.")
})

test_that("check_missingness returns NA when tip not in gene", {
  genes <- c("BacA", "CorA")
  tips <- c("GCA_000025925.1", "GCA_000144405.1", "fake_tip")
  path <- paste0(system.file("faa/", package = "groves"), "/")
  miss <- check_missingness(gene_names = genes,
                            tip_names = tips, 
                            path = path, 
                            tail = "_aln.faa")
  expect_true(is.na(miss[1, "fake_tip"]))
})
