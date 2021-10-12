#' @title Graph to check the 80-20 rule (Pareto principle)
#' 
#' @description Make a graph of the percentage of more connected nodes
#' as a function of the percentage of the total (weighted) indegree and outdegree
#' 
#' @param A An adjacency \code{\link[base]{matrix}}. 
#' @param xlabel The text for the x-axis label.
#' @param ylabel The text for the y-axis label.
#' @param legin The text for the legend of the indegree.
#' @param legin The text for the legend of the outdegree.
#' 
#' @details This function sorts in descreasing order 
#' the indegree and outdegree of the nodes of a netowrk.
#' Then the function calculates the percentage of more connected nodes 
#' and the corresponding percentage of the total indegree and outdegree.
#' The indegree and outdegree can be weighted or not, 
#' depending on the adjacency matrix provided by the user.
#'   
#' 
#' @return A graph of the percentage of more connected nodes
#' as a function of the percentage of the total (weighted) indegree and outdegree
#' for the user to check if the 80-20 rule (Pareto principle) 
#' applies to the analyzed network.
#' 
#' 
#' @references 
#' [1] Negreiros RL, Grisi-Filho JHH,  Dias RA, Ferreira F, Ferreira Neto JS, 
#' Ossada R, Amaku M (2020). 
#' "Analysis of the Cattle Trade Network in the State of Mato Grosso, Brazil."
#' Brazilian Journal of Veterinary Research and Animal Science, 57 (4), e171635.
#' \doi{/10.11606/issn.1678-4456.bjvras.2020.171635}
#' 
#' 
#' @export
#' @examples 
#' # Generate an arbitrary 100 by 100 adjacency matrix with zeros and ones
#' # Remove loops
#' A <- matrix(rbinom(100 * 100, 1, 0.2), ncol = 100, nrow = 100)
#' diag(A) <- 0
#' 
#' # Call function 
#' pareto(A)
#' 

pareto <- function(A, xlabel = "More connected nodes (%)",
                       ylabel = "% of total", legin = "in", legout = "out")
{
  # Loading library ggplot2:
  require(ggplot2)
  
  # Calculating indegree and outdegree
  kwin <- colSums(A)
  kwout <- rowSums(A)
  
  # Sorting in decreasing order
  skin <- sort(kwin,decreasing=TRUE)
  skout <- sort(kwout,decreasing=TRUE)
  
  # Cumulative values
  cumin <- cumsum(skin) 
  cumout <- cumsum(skout) 
  
  # Cumulative distribution (in %)
  pcumin <- cumin/(cumin[length(cumin)]) 
  pcumout <- cumout/(cumout[length(cumout)])
  
  # x-axis
  # Main hubs first
  hin <- (1:length(pcumin))/(length(pcumin))
  hout <-(1:length(pcumout))/(length(pcumout))
  
  # Stacking the data
  hstack <- append(hin,hout)
  pcstack <- append(pcumin,pcumout)
  legend <- c(rep(legin,length(hin)),rep(legout,length(hout)))
  
  # Creating a dataframe
  df <- data.frame(hstack,pcstack,legend)
  
  # Line graph in ggplot2 
  fig <- ggplot(df,aes(x=hstack*100, y=pcstack*100, group=legend, linetype=legend)) +
    geom_line(size=1) +
    theme(axis.text.x = element_text(size=16, colour="gray25"),
          axis.text.y = element_text(size=16, colour="gray25"),
          axis.title.x = element_text(colour="black", size=16, vjust=0.1), 
          axis.title.y = element_text(colour="black", size=16, vjust=0.3),
          legend.text = element_text(size=16),
          legend.position=c(0.8,0.2), legend.title=element_blank()) +
    scale_y_continuous(breaks=c(0,20,40,60,80,100)) + 
    scale_x_continuous(breaks=c(0,20,40,60,80,100)) + 
    xlab(xlabel) +
    ylab(ylabel) 
  
  return(fig)
}