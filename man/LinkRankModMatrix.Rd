\name{LinkRankModMatrix}
\alias{LinkRankModMatrix}
\title{#' Linkrank Modularity Matrix}
\usage{
LinkRankModMatrix(L, pr)
}
\arguments{
  \item{L}{LinkRank Matrix. Output of
  \code{\link{LinkRank}} function}

  \item{pr}{PageRank vector. Output of
  \code{\link{PageRank}} function}
}
\value{
LinkRank Modularity Matrix, where [i,j] is the contribution
to LinkRank Modularity if "i" and "j" are assigned to the
same community
}
\description{
Generates a matrix with contributions to LinkRank
Modularity, according to Kim (2010)
}
\details{
Complicated function... see the paper!
}
\examples{
# Generate LinkRank Modularity Matrix
LinkRankModMatrix(L,pr)
}
\references{
Kim (2010)

\url{http://github.com/leb-fmvz-usp.github/epinemo}
}
