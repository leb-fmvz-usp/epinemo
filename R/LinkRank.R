#' LinkRank Matrix
#' @description Calculates the LinkRank Matrix, according to Kim et al. [1]
#' @param G Google Matrix. Output of \code{\link{GoogleMatrix}} function
#' @param pr PageRank vector. Output of \code{\link{PageRank}} function
#' @return LinkRank Matrix
#' @details Complicated function... see the paper!
#' 
#' @references 
#' [1] Kim Y, Son SW, Jeong H (2010). 
#' "Finding Communities in Directed Networks." 
#' Physical Review E 81, 016103.
#' \doi{10.1103/PhysRevE.81.016103}
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
