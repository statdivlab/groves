test_that("alignments to cat works", {
  gene_names <- c("BacA", "CorA")
  path_from <- paste0(system.file("faa/", package = "groves"), "/")
  path_to <- "testing_alignments_to_cat"
  alignments_to_cat(target_genes = gene_names, 
                    path_from = path_from, 
                    tail = "_aln.faa",
                    path_to = path_to)
  expect_equal(length(list.files(path_to)), 2)
  unlink(path_to, recursive = TRUE)
})
