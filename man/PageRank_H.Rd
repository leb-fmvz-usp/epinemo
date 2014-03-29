\name{PageRank_H}
\alias{PageRank_H}
\title{PageRank of a Row Normalized Hyperlink Matrix}
\usage{
PageRank_H(H, alpha = 0.85, epsilon = 1e-08, pr0, v)
}
\arguments{
  \item{H}{\code{\link{matrix}} Row Normalized Hyperlink
  Matrix. N-by-N sparse matrix. Output of
  \code{\link{Rnhm}} function.}

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
Calculates the PageRank for an n-by-n Markov Matrix (Row
Normalized Hyperlink Matrix).
}
\details{
This function uses the Power method.
}
\examples{
# Calculates PageRank
PageRank_H(rnhm(A))
}
\references{
Livro do PageRank

\url{http://github.com/leb-fmvz-usp.github/epinemo}
}

