#' LinkRank Matrix
#' @description Calculates the LinkRank Matrix, according to Kim et al. [1].
#' @param G Google Matrix. Output of \code{\link{GoogleMatrix}} function
#' @param pr PageRank vector. Output of \code{\link{PageRank}} function
#' @return LinkRank Matrix
#' @details "Similar to the definition of PageRank, LinkRank
#' of a particular link should be equal to the probability that
#' a random walker follows the link from node \emph{i} to node \emph{j}" [1].
#' LinkRank from node \emph{i} to node \emph{j} can be defined as
#' \eqn{L_{ij} = \pi_{i} * G_{ij}}, where  
#' \eqn{\pi_{i}} is the \emph{i}th element of the PageRank vector,
#' and \eqn{G_{ij}} is the element of the Google Matrix.
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
