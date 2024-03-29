#' PageRank of a Network Matrix
#' @description Calculates the PageRank for a network matrix.
#' @param A network adjacency \code{\link{matrix}} 
#' @param alpha scaling parameter in PageRank model. It must be \code{\link{numeric}} between 0 and 1 inclusive. Default = 0.85.
#' @param epsilon convergence tolerance. Default = 1e-8. 
#' @param pr0 starting vector at iteration 0 (a row vector). Usually set to rep(1/n,n).
#' @param v personalization vector. sum(v) = 1.
#' @return Numeric vector with nodes' PageRank
#' @details This function uses the power method to calculate the PageRank
#' of a network matrix [1].
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
#' # Calculates PageRank
#' pageRank(A)

pageRank <- function(A,alpha=0.85,epsilon=1e-8,pr0,v)
{
  n <- dim(A)[1]
  if (missing(pr0))
    pr0 <- rep(1/n,n)
  if (missing(v))
    v <- rep(1/n,n)
  pr <- PageRank_H(Rnhm(A), alpha=alpha, epsilon=epsilon, pr0=pr0, v=v)
  return(pr)
}
