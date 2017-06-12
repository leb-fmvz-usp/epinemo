#' @title Calculate parameters related to the friendship paradox
#' 
#' @description Function to calculate friendship paradox metrics
#' 
#' @param A an adjacency \code{\link{matrix}}
#' 
#' @details Function to calculate friendship paradox metrics
#' 
#' @return \code{\link{list}}. The first vector,
#'         \code{$degree}, indicates the type of degree calculated,
#'         \code{$kmean}, mean degree,
#'         \code{$kff}, mean number of friends of friends,
#'         \code{ratio}, ratio between kff and kmean,
#'         \code{variance}, the variance of degree distribution,
#'         \code{difference}, the difference between the mean degree of friends and the 
#'         mean degree.
#' 
#' @references 
#' [1] Feld SL (1991). 
#'     Why Your Friends Have More Friends than You Do. 
#'     American Journal of Sociology 96, 1464–1477.
#'     
#' [2] Amaku M, Cipullo RI, Grisi-Filho JHH, Marques FS, Ossada R (2014). 
#'     The Friendship Paradox in Scale-Free Networks. 
#'     Applied Mathematical Sciences, 8 (37), 1837-1845. doi:10.12988/ams.2014.4288
#' 
#' 
#' \url{http://github.com/leb-fmvz-usp.github/epinemo}
#' @export
#' @examples 
#' # Loading data from....
#' 
#' # call function
#' CalculateFP(A)
#'                                                    


friendshipParadox <- function(A)
{
  require(Matrix)
  # Unweighted adjacency matrix (element=1 if there is a link)
  Abin <- 1*(A!=0)    

  # Indegree, Outdegree and Degree
  kin <- colSums(Abin) 
  kout <- rowSums(Abin) 
  matriz_vizinhos_nao_direcionada <- ( (Abin + t(Abin)) > 0) * 1
  k <- rowSums(matriz_vizinhos_nao_direcionada)

  # Mean <k> and variance
  kmean <- c(mean(kin),mean(kout),mean(k))
  kvariance <- c(var(kin),var(kout),var(k))
  
  # Mean number of friends of friends <kff>
  # Difference diff = <kff> - <k>
  kff <- c(mean(kin)+(var(kin)/mean(kin)), mean(kout)+(var(kout)/mean(kout)), mean(k)+(var(k)/mean(k)))
  diff <- c(var(kin)/mean(kin), var(kout)/mean(kout), var(k)/mean(k))
  
  # Ratio <kff>/<k>
  ratio <- kff/kmean
  
  # Row identification
  id <- c("kin","kout","k")
  
  return(list(degree=id, kmean=kmean, kff=kff, ratio=ratio, variance=kvariance, difference=diff))
}