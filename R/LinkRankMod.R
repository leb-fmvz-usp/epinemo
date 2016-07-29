#' Linkrank Modularity
#' @description Computes the Link Rank Modularity (Qlr), as described by Kim (2010)
#' @param L LinkRank Matrix. Output of \code{\link{LinkRank}} function
#' @param pr PageRank vector. Output of \code{\link{PageRank}} function
#' @param c partition vector
#' @return LinkRank Modularity value
#' @details Complicated function... see the paper!
#' 
#' @references Kim (2010)
#' 
#' \url{http://github.com/leb-fmvz-usp.github/epinemo}
#' @export
#' @examples 
#' # Calculates LinkRank Modularity
#' LinkRankMod(L,pr,c)

LinkRankMod <- function(L,pr,c)
{
  qlr <- 0
  lc <- unique(c)
  for (i in 1:length(lc))
  {
    ca <- c==lc[i]
    if (sum(ca) == 1) next()
    LA <- L[ca,ca]
    diag(LA) <- 0
    pra <- pr[ca]
    prMA <- pra%o%pra
    diag(LA) <- 0
    diag(prMA) <- 0
    qlr <- qlr + sum(LA) - sum (prMA)
  }
  return(qlr)
}

