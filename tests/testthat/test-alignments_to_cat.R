test_that("alignments to cat works", {
  gene_names <- c("BacA", "CorA")
  path_from <- paste0(system.file("faa/", package = "groves"), "/")
  alignments_to_cat(target_genes = gene_names, 
                    path_from = path_from, 
                    concat_name = "BacA_CorA_concat",
                    tail = "_aln.faa")
  expect_true(file.exists(paste0(path_from, "BacA_CorA_concat_aln.faa")))
})
