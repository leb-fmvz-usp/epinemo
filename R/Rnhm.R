#' Row-Normalized Hyperlink Matrix
#' @description This function creates a sparse row-normalized hyperlink matrix.
#' @param A network adjacency \code{\link{matrix}} 
#' @return row-normalized hyperlink matrix
#' @details This function creates a sparse row-normalized hyperlink matrix, 
#' defined in more detail in [1].
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
#' # Calculate a row-normalized hyperlink matrix
#' Rnhm(A)

Rnhm <- function(A)
{
  n  <- nrow(A)
  H  <- Matrix::Matrix(data = 0, nrow = n, ncol = n, sparse = T)
  Srow  <- rowSums(A)
  zerorows <- Srow == 0
  Srow[zerorows] <- 1
  H <- A / Srow
  return(H)
}
