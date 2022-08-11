test_that("remove_genomes works", {
  gene_set <- c("BacA", "CorA")
  genome_set <- c("GCA_000025925.1", "GCA_000144405.1", "fake_tip")
  faa_path <- paste0(system.file("faa/", package = "groves"), "/")
  faa_tail <- "_aln.faa"
  suff <- "_fewer_tips"
  remove_genomes(target_genes = gene_set, target_genomes = genome_set,
                  path = faa_path, tail = faa_tail, new_file_suffix = suff)
  expect_true(file.exists(paste0(faa_path, gene_set[1], suff, faa_tail)))
})
