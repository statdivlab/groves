test_that("genes_missing_outgroup works", {
  gene_set <- paste0("gene_", 1:3)
  faa_path <- paste0(system.file("faa/", package = "groves"), "/")
  pres <- get_presence(gene_names = gene_set, path = faa_path, tail = ".faa")
  expect_equal(genes_missing_outgroup(pres, "tip_1"), "gene_2")
})

test_that("genes_missing_outgroup throws error", {
  gene_set <- paste0("gene_", 1:3)
  faa_path <- paste0(system.file("faa/", package = "groves"), "/")
  pres <- get_presence(gene_names = gene_set, path = faa_path, tail = ".faa")
  expect_error(genes_missing_outgroup(pres, "tip_6"), 
               "Outgroup not included in presence matrix.")
})

