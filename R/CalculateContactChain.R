#' @title Calculates contact chain.
#' 
#' @description Function to calculate outgoing and ingoing contact
#'              chains.
#' 
#' @param Data \code{\link{data.frame}} with network information : node ID, origin
#'        node, destiny node, and the time in which the link was established.
#' 
#' @param from \code{\link{character}}, indicates the column used to characterize
#'        the origin node of each link.
#' 
#' @param to \code{\link{character}}, indicates the column used to characterize
#'        the destiny node of each link.
#' 
#' @param Time \code{\link{character}}, indicates the column used to characterize
#'        the time in which the link was established.
#' 
#' 
#' @details This is a function that calculates the contact chain of a dynamic
#'          network.
#' 
#' @return \code{\link{data.frame}}. The first column,
#'         \code{$id}, is the original ID of each node,
#'         \code{$ingoing}, is the ingoing contact chain value, and
#'         \code{$outgoing}, is the outgoing contact chain value.
#' 
#' @references 
#' [1] K Buttner, J Krieter, and I Traulsen. “Characterization of Contact Structures
#'     for the Spread of Infectious Diseases in a Pork Supply Chain in Northern
#'     Germany by Dynamic Network Analysis of Yearly and Monthly Networks.” In:
#'     Transboundary and emerging diseases 2000 (May 2013), pp. 1–12.
#' [2] C Dube, C Ribble, D Kelton, et al. “Comparing network analysis measures to
#'     determine potential epidemic size of highly contagious exotic diseases in
#'     fragmented monthly networks of dairy cattle movements in Ontario, Canada.”
#'     In: Transboundary and emerging diseases 55.9-10 (Dec. 2008), pp. 382–392.
#' [3] C Dube, C Ribble, D Kelton, et al. “A review of network analysis terminology
#'     and its application to foot-and-mouth disease modelling and policy development.”
#'     In: Transboundary and emerging diseases 56.3 (Apr. 2009), pp. 73–85.
#' [4] Jenny Frossling, Anna Ohlson, Camilla Bj ̈orkman, et al. “Application of
#'     network analysis parameters in risk-based surveillance - Examples based
#'     on cattle trade data and bovine infections in Sweden.” In:  Preventive
#'     veterinary medicine 105.3 (July 2012), pp. 202–208. doi: 10.1016/j.prevetmed.2011.12.011.
#' [5] Maria Noremark, Nina Ha kansson, Susanna Sternberg Lewerin, et al.
#'     “Network analysis of cattle and pig movements in Sweden: measures relevant
#'     for disease control and risk based surveillance.” In: Preventive veterinary
#'     medicine 99.2-4 (2011), pp. 78–90. doi: 10.1016/j.prevetmed.2010.12.009.
#' 
#' \url{http://github.com/leb-fmvz-usp.github/epinemo}
#' @export
#' @examples 
#' # Loading data from....
#' 
#' # call contact chain function
#' chain <- CalculateContactChain(Data, from, to, Time)
#'                                                    
CalculateContactChain <- function (Data, from, to, Time)
{
  require(Matrix)
  #check
  stopifnot(class(Data[,Time]) == "Date")
  #Create new IDs
  data <- Data[, c(from, to, Time)]
  data <- CreateUniqueIds(data = data, from = from, to = to)
  #Dimensions of Matrix
  dimensions <- rep( max(data$correspondence$new_id), 2)
  
  #Break movements by Date
  mov.list <- split(data$movements, data$movements[, Time])
  
  #Sparse Matrix for each Date
  net.array <- lapply(mov.list, function(x) sparseMatrix(i = x[, 'originID'], j = x[, 'destinyID'], x=1, dims= dimensions ))
  matrix.ccc <- Matrix(0, nc=dimensions[1], nr=dimensions[2], sparse=T)
  matrix.ccc <- as(matrix.ccc, "dgCMatrix")
  for (i in 1:length(net.array))
  {
    matrix.ccc <- matrix.ccc + net.array[[i]] + matrix.ccc %*% net.array[[i]]
    matrix.ccc[ matrix.ccc>0 ] <- 1
  }
  data <- data.frame(ID = data$correspondence$old_id)
  data$ingoing <- colSums(matrix.ccc)
  data$outgoing <- rowSums(matrix.ccc)
  return(data)
  diag(matrix.ccc) <- 0
  data <- data.frame(id = data$correspondence$old_id)
  data$ingoing <- colSums(matrix.ccc)
  data$outgoing <- rowSums(matrix.ccc)
  return(data)
}
