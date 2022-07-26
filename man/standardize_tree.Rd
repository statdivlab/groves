% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standardize_tree.R
\name{standardize_tree}
\alias{standardize_tree}
\title{Standardizes branch lengths for a single tree
For a single tree, divides each branch length by the sum of branch lengths or 
maximum branch length for that tree or sets all branch lengths to \code{1}.}
\usage{
standardize_tree(tree, denom = "branch_sum")
}
\arguments{
\item{tree}{The tree of interest.}

\item{denom}{The quantity to standardize by. Default is \code{branch_sum}, which is
the sum of branch lengths in that tree. Another option is \code{max_branch}, which
will divide each branch length by the maximum branch length in that tree. The last 
option is \code{one}, which recodes all branches to have length \code{1}.}
}
\value{
The standardized tree.
}
\description{
Standardizes branch lengths for a single tree
For a single tree, divides each branch length by the sum of branch lengths or 
maximum branch length for that tree or sets all branch lengths to \code{1}.
}
\examples{
path <- system.file("txt", "small_tree_set.txt", package = "groves")
tree_set <- ape::read.tree(path)
new_tree <- standardize_tree(tree_set[[1]])

}
