#' @title Calculates contact chain
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
#' @param simultaneous \code{\link{logical}}, wether movements within 
#'        the same time stamp (same day) are simultaneous (\code{\link{TRUE}})
#'        with no indirect contacts within each day or 
#'        continuous (\code{\link{FALSE}}) and indirect contacts between movements
#'         from the same day are possible.
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
#' [1] Buttner K, Krieter J, Traulsen I (2015). "Characterization of Contact Structures
#'     for the Spread of Infectious Diseases in a Pork Supply Chain in Northern
#'     Germany by Dynamic Network Analysis of Yearly and Monthly Networks."
#'     Transboundary and Emerging Diseases, 62, 188-199.
#'     \doi{10.1111/tbed.12106}
#'     
#' [2] Dube C, Ribble C, Kelton D, et al. (2008). "Comparing Network Analysis Measures to
#'     Determine Potential Epidemic Size of Highly Contagious Exotic Diseases in
#'     Fragmented Monthly Networks of Dairy Cattle Movements in Ontario, Canada."
#'     Transboundary and Emerging Diseases, 55 (9-10), 382-392.
#'     \doi{10.1111/j.1865-1682.2008.01053.x}
#'     
#' [3] Dube C, Ribble C, Kelton D, et al. (2009). "A Review of Network Analysis Terminology
#'     and its Application to Foot-and-Mouth Disease Modelling and Policy Development."
#'     Transboundary and Emerging Diseases, 56 (3), 73-85.
#'     \doi{10.1111/j.1865-1682.2008.01064.x}
#'     
#' [4] Frossling J, Ohlson A, Bjorkman C, et al. (2012). "Application of
#'     Network Analysis Parameters in Risk-Based Surveillance - Examples Based
#'     on Cattle Trade Data and Bovine Infections in Sweden." 
#'     Preventive Veterinary Medicine, 105 (3), 202-208. 
#'     \doi{10.1016/j.prevetmed.2011.12.011}
#'     
#' [5] Noremark M, Hakansson N, Lewerin SS, et al. (2011).
#'     "Network Analysis of Cattle and Pig Movements in Sweden: Measures Relevant
#'     for Disease Control and Risk Based Surveillance." 
#'     Preventive Veterinary Medicine, 99 (2-4), 78-90. 
#'     \doi{10.1016/j.prevetmed.2010.12.009}
#' 
# \url{http://github.com/leb-fmvz-usp.github/epinemo}
#' @export
#' @examples 
#' # Loading data from....
#' 
#' # call contact chain function
#' chain <- calculateContactChain(Data, from, to, Time)
#'                                                    
calculateContactChain <- function (Data, from, to, Time, simultaneous=T)
{
  require(Matrix)
  #check
  stopifnot(class(Data[,Time]) == "Date")
  #Create new IDs
  Data <- Data[, c(from, to, Time)]
  Data <- createUniqueIds(data = Data, from = from, to = to)
  #Dimensions of Matrix
  dimensions <- rep( max(Data$correspondence$network.id), 2)
  
  #Break movements by Date
  mov.list <- split(Data$movements, Data$movements[, Time])
  
  #Sparse Matrix for each Date
  net.array <- lapply(mov.list, function(x) sparseMatrix(i = x[, 'From'], j = x[, 'To'], dims= dimensions ))
  matrix.ccc <- Matrix(0, nc=dimensions[1], nr=dimensions[2], sparse=T)
  matrix.ccc <- as(matrix.ccc, "dgCMatrix")
  if (simultaneous)
  {
    for (i in 1:length(net.array)) #for each day
    {
      matrix.ccc <- matrix.ccc + net.array[[i]] + matrix.ccc %*% net.array[[i]]
      matrix.ccc <- (matrix.ccc > 0) * 1      
    }
  } else
  {
    for (i in 1:length(net.array)) #for each day
    {
      previous.chain <- day.chain <- indirect <- net.array[[i]] %*% net.array[[i]]
      if (sum(indirect) != 0)
      {
        repeat
        {
          indirect <- indirect %*% net.array[[i]] #possibly new indirect connections
          day.chain <- day.chain + indirect
          day.chain <- (day.chain>0)*1
          if ( identical(day.chain, previous.chain)) break() #no new connections made
          previous.chain <- day.chain
        }
      }
      day.chain <- net.array[[i]] + day.chain
      matrix.ccc <- matrix.ccc + day.chain + matrix.ccc %*% day.chain
      matrix.ccc <- (matrix.ccc > 0) * 1
    }
  }
  diag(matrix.ccc) <- 0
  Data <- data.frame(id = Data$correspondence$database.id)
  Data$ingoing <- colSums(matrix.ccc)
  Data$outgoing <- rowSums(matrix.ccc)
  return(Data)
}
