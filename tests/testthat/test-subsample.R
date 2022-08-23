test_that("subsample works", {
  gene_set <- paste0("gene_", 1:3)
  faa_path <- paste0(system.file("faa/", package = "groves"), "/")
  expect_warning(subsample(gene_names = gene_set, path = faa_path, tail = ".faa"),
                 "Only one genome selected. You can look manually for a complete set but
             this dataset may not be well-suited for gene tree exploration.")
})

test_that("subsample throws error when no shared genes", {
  gene_set <- paste0("gene_", 1:3)
  faa_path <- paste0(system.file("faa/", package = "groves"), "/")
  expect_error(subsample(gene_names = gene_set, path = faa_path, tail = ".faa",
                         genomes_to_keep = c("tip_4", "tip_3")),
               "There are no genes in all genomes provided as 
           genomes_to_keep.")
})

test_that("subsample throws error when no shared genomes", {
  gene_set <- paste0("gene_", 1:3)
  faa_path <- paste0(system.file("faa/", package = "groves"), "/")
  expect_error(subsample(gene_names = gene_set, path = faa_path, tail = ".faa",
                         genes_to_keep = c("gene_2", "gene_3")),
               "There are no genomes that share all genes provided as 
           genes_to_keep.")
})

test_that("subsample throws error when gene not in set", {
  gene_set <- paste0("gene_", 1:3)
  faa_path <- paste0(system.file("faa/", package = "groves"), "/")
  expect_error(subsample(gene_names = gene_set, path = faa_path, tail = ".faa",
                         genes_to_keep = c("gene_2", "gene_30")),
               "Some genes in genes_to_keep do not appear in your presence matrix. 
           Check gene_names to see which genes appear in your data.")
})

test_that("subsample throws error when genome not in set", {
  gene_set <- paste0("gene_", 1:3)
  faa_path <- paste0(system.file("faa/", package = "groves"), "/")
  expect_error(subsample(gene_names = gene_set, path = faa_path, tail = ".faa",
                         genomes_to_keep = c("tip_1", "tip_10")),
               "Some genomes in genomes_to_keep do not appear in your presence matrix. 
           You can run get_presence() to identify genomes that appear in your data.",
               fixed = TRUE)
})

test_that("subsample throws error when genes_to_keep only have one genome in common", {
  gene_set <- paste0("gene_", 1:3)
  faa_path <- paste0(system.file("faa/", package = "groves"), "/")
  expect_error(subsample(gene_names = gene_set, path = faa_path, tail = ".faa",
                         genes_to_keep = c("gene_1", "gene_3")),
               "Only one genome selected. You can look manually for a complete set but
             this dataset may not be well-suited for gene tree exploration.")
})

test_that("subsample throws error when genomes_to_keep only have one gene in common", {
  gene_set <- paste0("gene_", 1:3)
  faa_path <- paste0(system.file("faa/", package = "groves"), "/")
  expect_error(subsample(gene_names = gene_set, path = faa_path, tail = ".faa",
                         genomes_to_keep = c("tip_1", "tip_2")),
               "Only one gene selected. You can look manually for a complete set but
             this dataset may not be well-suited for gene tree exploration.")
})


