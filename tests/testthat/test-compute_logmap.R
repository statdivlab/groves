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
               "The base tree that you chose is not binary (it is unresolved). Please choose another base tree or let groves calculate a base tree for you by leaving out the base_lab argument.",
               fixed = TRUE)
})

test_that("compute_logmap gives error if all trees are on boundary", {
  set.seed(1)
  path <- paste0(system.file("txt", package = "groves"), "/")
  tree_paths <- paste0(path, "boundary_tree", 1:3, ".txt")
  trees <- ape::rmtree(3, 10)
  for (i in 1:length(trees)) {
    trees[[i]]$edge.length[1:17] <- 0 
    ape::write.tree(trees[[i]], tree_paths[i])
  }
  expect_error(compute_logmap(tree_paths = tree_paths,
                 tree_names = c("tree1", "tree2", "tree3")),
               "No trees in the tree set are binary (they are all unresolved). Because of this, this visualization tool cannot be run on this tree set.",
               fixed = TRUE)
})

test_that("compute_logmap chooses new tree if minimum distance tree is on boundary", {
  path <- paste0(system.file("txt", package = "groves"), "/")
  tree_paths <- c(paste0(path, "boundary_tree", 1:3, ".txt"),
                  paste0(path, "tree1.txt"))
  logmap <- compute_logmap(tree_paths = tree_paths)
  expect_equal(nrow(logmap$vectors), length(tree_paths))
})
