#' @title Makes a graph with Pareto's Law
#' 
#' @description Function to make a graph with Pareto's Law
#' 
#' @param A1 Adjacency \code{\link{matrix}} 
#' @param xlabel \code{\link{character}} with x-axis label
#' @param ylabel \code{\link{character}} with y-axis label
#' @param legin \code{\link{character}} with legend for K_in
#' @param legin \code{\link{character}} with legend for K_out
#' 
#' @details Graph with Pareto's Law
#' 
#' @return \code{\link{graph}} with Pareto's Law
#' 
#' @references 
#' 
#' \url{http://github.com/leb-fmvz-usp.github/epinemo}
#' @export
#' @examples 
#' 
#' pareto(A1)
#' 

pareto <- function(A1,xlabel="More connected nodes (%)",
                       ylabel="% of total",legin="in",legout="out")
{
  # Loading library ggplot2:
  require(ggplot2)
  
  # Calculating indegree and outdegree
  kwin <- colSums(A1)
  kwout <- rowSums(A1)
  
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