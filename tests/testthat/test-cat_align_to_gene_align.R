test_that("alignments to cat works", {
  gene_names <- c("BacA", "CorA")
  path_from <- paste0(system.file("faa/", package = "groves"), "/")
  cat_align_to_gene_align(concat_path = paste0(path_from, "BacA_CorA_concat_aln.faa"),
                          gene_names = gene_names,
                          output_path = paste0(path_from, "/gene_alignments/"))
  BacA_align1 <- readLines(paste0(path_from, "BacA_aln.faa"))
  BacA_align2 <- readLines(paste0(path_from, "gene_alignments/BacA.faa"))
  expect_true(all.equal(BacA_align1, BacA_align2))
})
