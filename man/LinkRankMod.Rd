\name{LinkRankMod}
\alias{LinkRankMod}
\title{Linkrank Modularity}
\usage{
LinkRankMod(L, pr, c)
}
\arguments{
  \item{L}{LinkRank Matrix. Output of
  \code{\link{LinkRank}} function}

  \item{pr}{PageRank vector. Output of
  \code{\link{PageRank}} function}

  \item{c}{partition vector}
}
\value{
LinkRank Modularity value
}
\description{
Computes the Link Rank Modularity (Qlr), as described by
Kim (2010)
}
\details{
Complicated function... see the paper!
}
\examples{
# Calculates LinkRank Modularity
LinkRankMod(L,pr,c)
}
\references{
Kim (2010)

\url{http://github.com/leb-fmvz-usp.github/epinemo}
}

