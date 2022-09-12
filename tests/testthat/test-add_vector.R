test_that("add_vector works", {
  path <- paste0(system.file("txt", package = "groves"), "/")
  tree_names <- c("tree1", "tree2", "tree3")
  tree_paths <- paste0(path, "tree", 1:3, ".txt")
  lm_res <- compute_logmap(tree_paths = tree_paths, tree_names = tree_names)
  new <- paste0(path, "tree4.txt")
  base <- tree_paths[which(tree_names %in% lm_res$base_lab)]
  res <- add_vector(new_tree_path = new, base_path = base, vectors = lm_res$vectors,
             new_name = "tree4")
  expect_equal(res[1:3, ], lm_res$vectors)
})

test_that("add_vector works with numbers as rownames of lm_res$vectors", {
  path <- paste0(system.file("txt", package = "groves"), "/")
  tree_names <- c("tree1", "tree2", "tree3")
  tree_paths <- paste0(path, "tree", 1:3, ".txt")
  lm_res <- compute_logmap(tree_paths = tree_paths)
  new <- paste0(path, "tree4.txt")
  base <- tree_paths[lm_res$base_lab]
  res <- add_vector(new_tree_path = new, base_path = base, vectors = lm_res$vectors,
                    new_name = "tree4")
  expect_equal(res[1:3, ], lm_res$vectors)
}) 

test_that("add_vector works with no new name given", {
  path <- paste0(system.file("txt", package = "groves"), "/")
  tree_names <- c("tree1", "tree2", "tree3")
  tree_paths <- paste0(path, "tree", 1:3, ".txt")
  lm_res <- compute_logmap(tree_paths = tree_paths, tree_names = tree_names)
  new <- paste0(path, "tree4.txt")
  base <- tree_paths[which(tree_names %in% lm_res$base_lab)]
  res <- add_vector(new_tree_path = new, base_path = base, vectors = lm_res$vectors)
  expect_equal(res[1:3, ], lm_res$vectors)
}) 
