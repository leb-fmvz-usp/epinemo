#' #' Linkrank Modularity Matrix
#' @description Generates a matrix with contributions to LinkRank Modularity, according to Kim (2010)
#' @param L LinkRank Matrix. Output of \code{\link{LinkRank}} function
#' @param pr PageRank vector. Output of \code{\link{PageRank}} function
#' @return LinkRank Modularity Matrix, where [i,j] is the contribution to LinkRank Modularity if "i" and "j" are assigned to the same community
#' @details Complicated function... see the paper!
#' 
#' @references Kim (2010)
#' 
#' \url{http://github.com/leb-fmvz-usp.github/epinemo}
#' @export
#' @examples 
#' # Generate LinkRank Modularity Matrix
#' LinkRankModMatrix(L,pr)

LinkRankModMatrix <- function(L,pr)
{
  n <- length(pr)
  qlrM <- matrix(0,n,n)
  prMA <- pr%o%pr
  for (i in 1:n)
  {
    LL <- L[i,]
    prL <- prMA[i,]
    for (j in 1:n)
    {
      qlrM[i,j] <- LL[j] - prL[j]
    }
  }
  diag(qlrM) <- 0;
  return(qlrM)
}
