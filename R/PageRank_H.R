#' PageRank of a Row Normalized Hyperlink Matrix
#' @description Calculates the PageRank for an n-by-n Markov Matrix (Row Normalized Hyperlink Matrix).
#' @param H \code{\link{matrix}} Row Normalized Hyperlink Matrix. N-by-N sparse matrix. Output of \code{\link{Rnhm}} function.
#' @param alpha scaling parameter in PageRank model. It must be \code{\link{numeric}} between 0 and 1 inclusive. Default = 0.85.
#' @param epsilon convergence tolerance. Default = 1e-8. 
#' @param pr0 starting vector at iteration 0 (a row vector). Usually set to rep(1/n,n).
#' @param v personalization vector. sum(v) = 1.
#' @return Numeric vector with nodes' PageRank
#' @details This function uses the power method to calculate
#' the PageRank of a row normalized hyperlink matrix [1].
#' 
#' @importFrom stats dist
#' 
#' @references 
#' [1] Langville AN, Meyer CD (2006). "Google's PageRank and Beyond: The Science 
#' of Search Engine Rankings." Princeton University Press, Princeton.
#' 
#' @export
#' @examples 
#' # Generate an arbitrary 100 by 100 adjacency matrix with zeros and ones
#' # Remove loops
#' A <- matrix(rbinom(100 * 100, 1, 0.2), ncol = 100, nrow = 100)
#' diag(A) <- 0
#' 
#' # Calculate PageRank
#' PageRank_H(Rnhm(A))

PageRank_H <- function(H,alpha=0.85,epsilon=1e-8,pr0,v)
{
  n <- dim(H)[1]
  if (missing(pr0))
    pr0 <- rep(1/n,n)
  if (missing(v))
    v <- rep(1/n,n)
  rowsumvector <- rowSums(H) == 0
  a <- rowsumvector*1
  residual <- 1
  pr <- pr0
  while (residual >= epsilon)
  {
    prevpr <- pr
    pr <- as.vector((alpha*pr)%*%H) + (alpha*(pr%*%a)+1-alpha)*v
    residual <- dist(rbind(pr,prevpr),method='manhattan')
  }
  return(pr)
}

