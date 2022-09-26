test_that("compute_logmap runs with no errors", {
  path <- paste0(system.file("txt", package = "groves"), "/")
  tree_paths <- paste0(path, "tree", 1:3, ".txt")
  logmap <- compute_logmap(tree_paths = tree_paths,
                tree_names = c("tree1", "tree2", "tree3"))
  expect_equal(nrow(logmap$vectors), length(tree_paths))
})

test_that("compute_logmap works without tree_names", {
  path <- paste0(system.file("txt", package = "groves"), "/")
  tree_paths <- paste0(path, "tree", 1:3, ".txt")
  logmap <- compute_logmap(tree_paths = tree_paths)
  expect_equal(nrow(logmap$vectors), length(tree_paths))
})

test_that("compute_logmap works with base_lab as name", {
  path <- paste0(system.file("txt", package = "groves"), "/")
  tree_paths <- paste0(path, "tree", 1:3, ".txt")
  logmap <- compute_logmap(tree_paths = tree_paths,
                           base_lab = "tree1",
                           tree_names = c("tree1", "tree2", "tree3"))
  expect_equal(nrow(logmap$vectors), length(tree_paths))
})

test_that("compute_logmap works with base_lab as number", {
  path <- paste0(system.file("txt", package = "groves"), "/")
  tree_paths <- paste0(path, "tree", 1:3, ".txt")
  logmap <- compute_logmap(tree_paths = tree_paths,
                           base_lab = 1,
                           tree_names = c("tree1", "tree2", "tree3"))
  expect_equal(nrow(logmap$vectors), length(tree_paths))
})

test_that("compute_logmap gives error if non-numeric base_lab isn't in tree_names", {
  path <- paste0(system.file("txt", package = "groves"), "/")
  tree_paths <- paste0(path, "tree", 1:3, ".txt")
  expect_error(compute_logmap(tree_paths = tree_paths,
                              base_lab = "tree_1",
                              tree_names = c("tree1", "tree2", "tree3")),
            "base_lab given does not appear in tree_names vector. Please let groves
             calculate a base tree for you, input a number corresponding your desired
             base tree's position in tree_paths, or a name that appears in tree_paths.")
})

test_that("compute_logmap gives error if base tree is on boundary", {
  path <- paste0(system.file("txt", package = "groves"), "/")
  tree_paths <- paste0(path, "tree", 1:3, ".txt")
  tree_paths <- c(tree_paths, paste0(path, "boundary_tree.txt"))
  expect_error(compute_logmap(tree_paths = tree_paths,
                              base_lab = "boundary_tree",
                              tree_names = c("tree1", "tree2", "tree3", 
                                             "boundary_tree")),
               "The chosen base tree, boundary_tree, is on the boundary of tree space and the log
                map cannot be computed using this tree. If you are manually choosing the
                base tree, please try another one. If this base tree was chosen automatically,
                you can try setting a base tree manually, but if you are unable to find a tree
                that does not return this error, this method may not work for your tree set.")
})
