#' @title Clustering Coefficient
#' 
#' @description A function to calculate the clustering coefficient for all nodes in a network.
#' 
#' @param A an adjacency \code{\link{matrix}} with network information.
#' @param directed \code{\link{logical}} whether to calculate the directed or undirected clustering coefficients.
#' 
#' @details This function calculates the inward and outward clustering coefficient in directed networks [1]. 
#' 
#' @return If directed = TRUE, a \code{\link{data.frame}}. The first column,
#'         \code{$clustering.in}, is the inward clustering coefficient,
#'         \code{$clustering.out}, is the outward clustering coefficient.
#'         If directed = FALSE, a \code{\link{vector}}, with the undirected clustering coefficient.
#' 
#' @references 
#' [1] Caldarelli G (2007). Scale-Free Networks. Oxford University Press, Oxford.
#' 
# \url{http://github.com/leb-fmvz-usp.github/epinemo}
#' @export
#' @examples 
#' # Loading data from....
#' 
#' # call contact chain function
#' cc <- clusteringCoefficient(A)
#'                                                    
clusteringCoefficient <- function(A, directed=T)
{
  if (directed)
  {
    A <- (A > 0) *1
    diag(A) <- 0
    kin <- colSums(A)
    kout <- rowSums(A)
    possible.triangles.in <- kin * (kin -1)
    possible.triangles.out <- kout * (kout - 1)
    triangles.matrix <- (A %*% A) * A
    triangles.in  <- colSums(triangles.matrix)
    triangles.out <- rowSums(triangles.matrix)
    clustering.in  <- triangles.in / possible.triangles.in
    clustering.out <- triangles.out / possible.triangles.out
    clustering.in[ is.nan(clustering.in)] <- NA
    clustering.out[ is.nan(clustering.out)] <- NA
    return(data.frame(clustering.in, clustering.out))
  } else
  {
    A <- A + t(A)
    A <- (A > 0) *1
    diag(A) <- 0
    k <- rowSums(A)
    possible.triangles <- k * (k-1) / 2
    triangles.matrix <- (A %*% A) * A
    triangles <- rowSums(triangles.matrix) / 2
    clustering <- triangles / possible.triangles
    return(clustering)
  }
}