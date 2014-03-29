#' Linkrank Matrix
#' @description Calculates the LinkRank Matrix, according to Kim (2010)
#' @param G Google Matrix. Output of \code{\link{GoogleMatrix}} function
#' @param pr PageRank vector. Output of \code{\link{PageRank}} function
#' @return LinkRank Matrix
#' @details Complicated function... see the paper!
#' 
#' @references Kim (2010)
#' 
#' \url{http://github.com/leb-fmvz-usp.github/epinemo}
#' @export
#' @examples 
#' # Calculates LinkRank Matrix
#' LinkRank(G,pr)

LinkRank <- function (G,pr)
{
  n <- length(pr)
  L <- matrix(0,n,n)
  for (i in 1:n)
    L[i,] <- pr[i] * G[i,]
  return(L)
}
