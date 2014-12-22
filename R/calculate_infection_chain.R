#' It calculates infection chain.
#' @description Parallel process to calculate outgoing and ingoing infection chain.
#' @param Network information data \code{\link{data.frame}} with: node ID, origin node, destiny node, and the time in which the link was established.
#' @param from \code{\link{character}}, indicates the column used to characterize the origin node of each link.
#' @param to \code{\link{character}}, indicates the column used to characterize the destiny node of each link.
#' @param Time \code{\link{character}}, indicates the column used to characterize the time in which the link was established.
#' selected.nodes \code{\link{vector}}, the infection chain will be calculated for the nodes in the selected.node vector.
#' type \code{\link{character}}, it determines which infection chain type will be calculated (default is 'outgoing'). 
#' number.of.cores \code{\link{integer}}, number of cores used to calculate the infection chain (default is 2).
#' close.connection \code{\link{character}}, whether the function closes all connections (default is TRUE). 
#' @details This is a function that calculates the infection chain of a dynamic network.
#' @return \code{\link{data.frame}}. The first column, \code{*$infection.chain}, is the infection chain value. The second column, \code{*$selected.nodes}, are the selected nodes.
#' @references 
#' [1] K Buttner, J Krieter, and I Traulsen. “Characterization of Contact Structures for the Spread of Infectious Diseases in a Pork Supply Chain in Northern Germany by Dynamic Network Analysis of Yearly and Monthly Networks.” In: Transboundary and emerging diseases 2000 (May 2013), pp. 1–12.
#' [2] C Dub ́e, C Ribble, D Kelton, et al. “Comparing network analysis measures to determine potential epidemic size of highly contagious exotic diseases in fragmented monthly networks of dairy cattle movements in Ontario, Canada.” In: Transboundary and emerging diseases 55.9-10 (Dec. 2008), pp. 382–392.
#' [3] C Dub ́e, C Ribble, D Kelton, et al. “A review of network analysis terminology and its application to foot-and-mouth disease modelling and policy development.” In: Transboundary and emerging diseases 56.3 (Apr. 2009), pp. 73–85.
#' [4] Jenny Frossling, Anna Ohlson, Camilla Bj ̈orkman, et al. “Application of network analysis parameters in risk-based surveillance - Examples based on cattle trade data and bovine infections in Sweden.” In:  Preventive veterinary medicine 105.3 (July 2012), pp. 202–208. doi: 10.1016/j.prevetmed.2011.12.011.
#' [5] Maria Noremark, Nina Ha kansson, Susanna Sternberg Lewerin, et al. “Network analysis of cattle and pig movements in Sweden: measures relevant for disease control and risk based surveillance.” In: Preventive veterinary medicine 99.2-4 (2011), pp. 78–90. doi: 10.1016/j.prevetmed.2010.12.009.
#' \url{http://github.com/leb-fmvz-usp.github/epinemo}
#' @export
#' @examples 
#' # Loading data from....
#' 
#' # call infection chain function
#' outgoing.infection.chai <- CalculateInfectionChain(Data, from, to, Time,
#'                                                    selected.nodes,
#'                                                    type = 'outgoing')
#'                                                    
#' ingoing.infection.chai <- CalculateInfectionChain(Data, from, to, Time,
#'                                                    selected.nodes,
#'                                                    type = 'outgoing')

CalculateInfectionChain <- function(Data, from, to, Time, selected.nodes,
                                    type = 'outgoing', number.of.cores = 2,
                                    close.connection = TRUE){
  
  #### Extracting, trasforming and loading the data base #####
  Data <- Data[,c(from,to,Time)]
  
  #### libraries #### 
  require(foreach)
  require(doSNOW)
  
  #### storage ####
  number.of.nodes <- length(unique(c(Data[, from],Data[, to])))
  infection.chain <- vector(length = number.of.nodes, mode = 'integer')
  new.node <- as.integer(1)
  
  #### parallel function algorithm ####
  DoInfectionChain <- function(){
    
    infection.chain1 <- infection.chain
    infection.chain1[selected.nodes[n]] <- new.node
    
    for(d in mov.time[-1]){
      
      infection.chain1[
        Data[which(Data[, Time] == d & Data[, from] %in%
                     which(infection.chain1 == 1)), to]] <- new.node
    }
    
    return(infection.chain1);  
  }
  
  #### Parallel call ####
  registerDoSNOW(makeCluster(number.of.cores, type = "SOCK")); # creating connection
  
  if(type == 'outgoing'){
    
    mov.time <- sort(unique(Data[,Time]))
    
    infection.chain <- foreach(n=1:length(selected.nodes),.verbose=FALSE, .combine = 'rbind',
                               .inorder=TRUE) %dopar% (DoInfectionChain())
    
    if(close.connection == TRUE)
      closeAllConnections()
    
    infection.chain <- apply(infection.chain,1,sum)
    infection.chain <- data.frame(infection.chain, selected.nodes)
    
    return(infection.chain)
    
  } else if(type == 'ingoing'){
    
    names(Data)[c(1,2)] <- c(to,from)
    mov.time <- sort(unique(Data[,Time]), decreasing = T)
    
    infection.chain <- foreach(n=1:length(selected.nodes),.verbose=FALSE, .combine = 'rbind',
                               .inorder=TRUE) %dopar% (DoInfectionChain())
    
    if(close.connection == TRUE)
      closeAllConnections()
    
    infection.chain <- apply(infection.chain,1,sum)
    infection.chain <- data.frame(infection.chain, selected.nodes)
    
    return(infection.chain)
    
  }
}
