#' LinkRank Modularity Matrix
#' @description Generates a matrix with contributions to LinkRank Modularity, according to Kim et al. [1].
#' @param L LinkRank Matrix. Output of \code{\link{LinkRank}} function
#' @param pr PageRank vector. Output of \code{\link{pageRank}} function
#' @return LinkRank Modularity Matrix, where the element \emph{[i,j]} 
#' is the contribution to LinkRank Modularity if nodes \emph{i} and \emph{j} are assigned to the same community
#' @details This function generates a matrix with
#' contributions to the LinkRank Modularity. 
#' A more detailed description can be found in [1].
#' 
#' @references 
#' [1] Kim Y, Son SW, Jeong H (2010). 
#' "Finding Communities in Directed Networks." 
#' Physical Review E 81, 016103.
#' \doi{10.1103/PhysRevE.81.016103}
#' 
#' @export
#' @examples 
#' # Generate an arbitrary 100 by 100 adjacency matrix with zeros and ones
#' # Remove loops
#' A <- matrix(rbinom(100 * 100, 1, 0.2), ncol = 100, nrow = 100)
#' diag(A) <- 0
#' 
#' # Calculate Google Matrix
#' G <- GoogleMatrix(A)
#' 
#' # Calculate PageRank vector
#' pr <- pageRank(A)
#' 
#' # Calculate LinkRank Matrix
#' L <- LinkRank(G,pr)
#' 
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
