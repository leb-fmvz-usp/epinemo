% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rnhm.R
\name{Rnhm}
\alias{Rnhm}
\title{Row-Normalized Hyperlink Matrix}
\usage{
Rnhm(A)
}
\arguments{
\item{A}{network adjacency \code{\link{matrix}}}
}
\value{
row-normalized hyperlink matrix
}
\description{
This function creates a sparse row-normalized hyperlink matrix.
}
\details{
This function creates a sparse row-normalized hyperlink matrix, 
defined in more detail in [1].
}
\examples{
# Generate an arbitrary 100 by 100 adjacency matrix with zeros and ones
# Remove loops
A <- matrix(rbinom(100 * 100, 1, 0.2), ncol = 100, nrow = 100)
diag(A) <- 0

# Calculate a row-normalized hyperlink matrix
Rnhm(A)
}
\references{
[1] Langville AN, Meyer CD (2006). "Google's PageRank and Beyond: The Science 
of Search Engine Rankings." Princeton University Press, Princeton.
}
