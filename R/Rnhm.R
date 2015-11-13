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
  n  <- nrow(A)
  H  <- Matrix(data = 0, nrow = n, ncol = n, sparse = T)
  Srow  <- rowSums(A)
  zerorows <- Srow == 0
  Srow[zerorows] <- 1
  H <- A / Srow
  return(H)
}
