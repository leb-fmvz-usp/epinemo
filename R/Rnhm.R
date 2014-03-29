#' row-normalized hyperlink matrix
#' @description This function creates a sparse row-normalized hyperlink matrix
#' @param A network adjacency \code{\link{matrix}} 
#' @return row-normalized hyperlink matrix
#' @details Complicated function... see the book!
#' 
#' @references Livro do PageRank
#' 
#' \url{http://github.com/leb-fmvz-usp.github/epinemo}
#' @export
#' @examples 
#' # Calculates rnhm
#' Rnhm(A)

Rnhm <- function(A)
{
  require(Matrix)
  Srow  <- rowSums(A)
  n  <- dim(A)[1]
  H  <- matrix(0,n,n)
  for (i in 1:n)
  {
    if (Srow[i] != 0)
      H[i,] <- A[i,]/Srow[i]
  }
  return(H)
}