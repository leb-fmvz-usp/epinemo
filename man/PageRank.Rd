\name{PageRank}
\alias{PageRank}
\title{PageRank of a Network Matrix}
\usage{
PageRank(A, alpha = 0.85, epsilon = 1e-08, pr0, v)
}
\arguments{
  \item{A}{network adjacency \code{\link{matrix}}}

  \item{alpha}{scaling parameter in PageRank model. It must
  be \code{\link{numeric}} between 0 and 1 inclusive.
  Default = 0.85.}

  \item{epsilon}{convergence tolerance. Default = 1e-8.}

  \item{pr0}{starting vector at iteration 0 (a row vector).
  Usually set to rep(1/n,n).}

  \item{v}{personalization vector. sum(v) = 1.}
}
\value{
Numeric vector with nodes' PageRank
}
\description{
Calculates the PageRank for a network matrix
}
\details{
This function uses the Power method.
}
\examples{
# Calculates PageRank
PageRank(A)
}
\references{
Livro do PageRank

\url{http://github.com/leb-fmvz-usp.github/epinemo}
}

