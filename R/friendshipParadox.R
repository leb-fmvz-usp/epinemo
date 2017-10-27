#' @title Friendship paradox metrics
#' 
#' @description Calculate parameters related to the friendship paradox
#' 
#' @param A an adjacency \code{\link{matrix}}
#' 
#' @details This function calculates friendship paradox metrics, that is, the mean degree (\code{kmean}),
#' the mean degree of friends (\code{kff}), the ratio between \code{kff} and \code{kmean} 
#' (\code{ratio}), the variance of the degree distribution (\code{variance}) and the difference 
#' between the mean degree of friends and the mean degree (\code{difference}).
#' 
#' 
#' @return A list of 
#' \item{degree}{the type of degree calculated: indegree (\code{kin}), outdegree (\code{kout}) 
#' and total degree (\code{k}).}
#' \item{kmean}{the mean number of friends (degree).}
#' \item{kff}{the mean number of friends of friends (degree of friends).}
#' \item{ratio}{the ratio between \code{kff} and \code{kmean}.}
#' \item{variance}{the variance of degree distribution.}
#' \item{difference}{the difference between \code{kff} and \code{kmean}.}
#'
#' 
#' @references 
#' Feld SL (1991). 
#' Why Your Friends Have More Friends than You Do. 
#' American Journal of Sociology 96, 1464-1477.
#'     
#' Amaku M, Cipullo RI, Grisi-Filho JHH, Marques FS, Ossada R (2014). 
#' The Friendship Paradox in Scale-Free Networks. 
#' Applied Mathematical Sciences, 8 (37), 1837-1845. doi:10.12988/ams.2014.4288
#' 
#' Amaku M, Grisi-Filho JHH, Negreiros RL, Dias RA, Ferreira F, Ferreira Neto JS,
#' Cipullo RI, Marques FS, Ossada R (2015). 
#' Infectious Disease Surveillance in Animal Movement Networks: 
#' An Approach Based on the Friendship Paradox.
#' Preventive Veterinary Medicine, 121, 306-313. doi:10.1016/j.prevetmed.2015.08.002
#' 
#' Amaku M, Grisi-Filho JHH (2015). The Friendship Paradox as a Strategy for Scenarios 
#' with Incomplete Network Data. Physics of Life Reviews, 15, 39-40. 
#' doi:10.1016/j.plrev.2015.07.006
#' 
#'     
#' 
#' @export
#' @examples 
#' require(Matrix)
#' 
#' # Generate an arbitrary 10 by 10 adjacency matrix with zeros and ones
#' # Remove loops
#' A <- matrix(rbinom(10 * 10, 1, 0.2), ncol = 10, nrow = 10)
#' diag(A) <- 0
#' 
#' # Call function
#' friendshipParadox(A)
#'                                                    


friendshipParadox <- function(A)
{
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