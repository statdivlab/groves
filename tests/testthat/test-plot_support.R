test_that("plot_support works", {
  trees <- ape::rmtree(100, 5, rooted = TRUE)
  support <- check_gene_support(trees[[1]], trees[2:100], rooted = TRUE)
  expect_type(plot_support(main_tree = trees[[1]], support = support, 
                           xlim_max = 2, show_legend = FALSE), "list")
})

test_that("plot_support works with coloring branches", {
  trees <- ape::rmtree(100, 5, rooted = FALSE)
  support <- c(1, 0.5, 0.2)
  expect_type(plot_support(main_tree = trees[[1]], support = support, 
                           xlim_max = 2, color_branch = TRUE, support_type = "boot"), 
              "list")
})

test_that("plot_support works with coloring branches", {
  trees <- ape::rmtree(100, 5, rooted = FALSE)
  support <- c(1, 0.5, 0.2)
  expect_type(plot_support(main_tree = trees[[1]], support = support, 
                           xlim_max = 2, color_branch = TRUE, support_type = "boot"), 
              "list")
})

test_that("plot_support works with provided title", {
  trees <- ape::rmtree(100, 5, rooted = FALSE)
  support <- c(1, 0.5, 0.2)
  expect_type(plot_support(main_tree = trees[[1]], support = support, 
                           xlim_max = 2, color_branch = TRUE, title = "title"), 
              "list")
})

test_that("plot_support works with other support_type", {
  trees <- ape::rmtree(100, 5, rooted = FALSE)
  support <- c(1, 0.5, 0.2)
  expect_type(plot_support(main_tree = trees[[1]], support = support, 
                           xlim_max = 2, color_branch = TRUE, support_type = "other"), 
              "list")
})

test_that("plot_support gives error if non-allowed support_type", {
  trees <- ape::rmtree(100, 5, rooted = FALSE)
  support <- c(1, 0.5, 0.2)
  expect_error(plot_support(main_tree = trees[[1]], support = support, 
                           support_type = "none"), 
              "Please enter 'gene', 'boot', or 'other' for support_type.")
})