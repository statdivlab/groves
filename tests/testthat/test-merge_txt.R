test_that("merge_txt works", {
  path <- paste0(system.file("txt", package = "groves"), "/")
  tree_paths <- paste0(path, "tree", 1:3, ".txt")
  output_path <- paste0(path, "test_trees.txt")
  merge_txt(paths = tree_paths, output_path = output_path)
  expect_equal(class(ape::read.tree(output_path)), "multiPhylo")
})
