#' This function creates the Google Matrix
#' @description Calculates the PageRank for a network matrix
#' @param A network adjacency \code{\link{matrix}} 
#' @param alpha scaling parameter in PageRank model. It must be \code{\link{numeric}} between 0 and 1 inclusive. Default = 0.85.
#' @return Google \code{\link{matrix}}.
#' @details This function is very simple.
#' 
#' @references 
#' [1] Page L, Brin S, Motwani R, Winograd T (1999). "The PageRank 
#' Citation Ranking: Bringing Order to the Web." Stanford Digital 
#' Library Technologies Project 1999.
#' \url{http://www.db.stanford.edu/~backrub/pageranksub.ps}
#' 
#' [2] Langville AN, Meyer CD (2006). "Google's PageRank and Beyond: The Science 
#' of Search Engine Rankings." Princeton University Press, Princeton.
#' 
#' \url{http://github.com/leb-fmvz-usp.github/epinemo}
#' @export
#' @examples 
#' # Calculates Google Matrix
#' Google.matrix(A)

GoogleMatrix <- function(A,alpha=0.85)
{
  Srow <- rowSums(A)
  n  <- dim(A)[1]
  G <- matrix(0,n,n)
  for (i in 1:n)
  {
    if (Srow[i] != 0)
      G[i,] <- alpha*(A[i,]/Srow[i]) + (1/n)*(1-alpha)
    else
      G[i,] <- 1/n
  }
  return(G)
}
