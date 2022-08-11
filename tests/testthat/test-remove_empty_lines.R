test_that("remove_empty_lines works", {
  gene_set <- c("BacA", "CorA")
  faa_path <- paste0(system.file("faa/", package = "groves"), "/")
  faa_tail <- "_aln_tips_miss.faa"
  suff <- "_clean"
  remove_empty_lines(target_genes = gene_set, path = faa_path, tail = faa_tail, 
                      new_file_suffix = suff)
  expect_true(file.exists(paste0(faa_path, gene_set[1], suff, faa_tail)))
})
