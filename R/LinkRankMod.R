#' LinkRank Modularity
#' @description Computes the LinkRank Modularity (\emph{Qlr}), as described by Kim et al. [1]
#' @param L LinkRank Matrix. Output of \code{\link{LinkRank}} function
#' @param pr PageRank vector. Output of \code{\link{pageRank}} function
#' @param c partition vector
#' @return LinkRank Modularity value
#' @details The LinkRank Modularity (\emph{Qlr}) is a modified modularity
#' for both directed and undirected networks, defined as [1]
#' \deqn{Qlr = (fraction of time spent walking within communities by a random walker)
#' - (expected value of this fraction).}
#' According to this definition, "a community is no longer a group of nodes
#' in which links are more densely located.
#' Instead, a community is a group of nodes in which a random walker
#' is more likely to stay" [1].
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
#' # Assign a partition vector
#' c <- 1:100
#' 
#' # Calculate LinkRank Modularity
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

