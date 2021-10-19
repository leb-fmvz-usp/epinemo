#' Linkrank Modularity Optimization
#' @description Computes the optimal partition of a network, based on LinkRank Modularity
#' @param qlrmM LinkRank Modularirty Matrix. Output of \code{\link{LinkRankModMatrix}} function
#' @param A Alternatively, the network adjacency matrix can be provided.
#' @param c Initial parition vector. Default to c = 1:n
#' @param Tc Initial temperature
#' @param minTc Minimal temperature
#' @param cool System cooling factor
#' @param max_itry Number of iterations in each temperature. Default to "n^2"
#' @param max_heat Maximal consecutive heat changes in each temperature. Default to "n".
#' @param max_rej Maximal consecutive rejection changes in each temperature. Default to "n".
#' @param plots Logical. If TRUE plots the progres of the algorithm. Requires network adjacency matrix and "statnet" package. Default to FALSE
#' @param location Directory to save the plots described above.
#' @return Numeric vector, with the optimal partition found.
#' @details Complicated function... see the paper! 
#' Uses Simulated Annealing (see reference) 
#' Only test simple movements between groups, doesn't test group movements.
#' 
#' @references 
#' [1] Kim Y, Son SW, Jeong H (2010). 
#' "Finding communities in directed networks." 
#' Physical Review E 81, 016103.
#' \doi{10.1103/PhysRevE.81.016103}
#' 
#' [2] Kirkpatrick S, Gelatt CDJ, Vecchi MP (1983). 
#' "Optimization by simulated annealing." 
#' Science 220 (May (4598)), 671-680.
#' \url{http://www.ncbi.nlm.nih.gov/pubmed/17813860}
#' 
#' \url{http://github.com/leb-fmvz-usp.github/epinemo}
#' @export
#' @examples 
#' # Calculates the optimal partition of a given network
#' linkRankOptimalPartition(A)

linkRankOptimalPartition <- function(qlrM,A,c,Tc=1,minTc=1e-10,cool=0.995,max_itry,max_heat,max_rej,plots=F,location=getwd())
{
#   Parameters
  if (missing(qlrM))
  {
    pr <- pageRank(A)
    G <- GoogleMatrix(A)
    L <- LinkRank(G,pr)
    qlrM <- LinkRankModMatrix(L,pr)
  }
  n <- dim(qlrM)[1]
  if (missing(c))
    c <- 1:n
  if (missing(max_itry))
    max_itry <- n^2
  if (missing(max_heat))
    max_heat <- 0.125 * max_itry
  if (missing(max_rej))
    max_rej <- max_itry
  lc <- unique(c)
  if (plots==T)
  {
    paleta <- colorRampPalette(c("black","red","green3","blue","cyan","magenta","yellow","gray","white","purple"))
    palette(paleta(length(lc)))
    netA <- as.network.matrix(as.matrix(A))
  }
#   Initial variables
  itry <- total <- heat <- rej <- concluded <- 0
  t_cicles <- round(log (minTc/Tc,base=cool))
  time1 <- proc.time()[3]
  while (Tc >= minTc & rej < max_rej)
  {
    itry <- itry+1
    # Draw a node and a community to exchange
    nsort <- sample(n,1)
    csort <- sample(lc[-which(lc==c[nsort])],1)
    # Then calculates the contribution of node 'n' when in one community, and when in another
    LqlrM <- qlrM[nsort,]
    CqlrM <- qlrM[,nsort]
    oldm <- sum(LqlrM[c==c[nsort]]) + sum(CqlrM[c==c[nsort]]) # current contribution of the drawn node in modularity
    newm <- sum(LqlrM[c==csort]) + sum(CqlrM[c==csort]) # future contribution of the drawn node in modularity
    # Now decide whether to switch or not
    if (newm>=oldm)
      {
      c[nsort] <- csort
      }
    else
      {
        if (runif(1) < exp((newm-oldm)/Tc))
          {
          c[nsort] <- csort
          heat <- heat + 1
          }
        else
          {
          rej <- rej + 1
          }
      }
    if (itry > max_itry | heat > max_heat)
      {
      r_cicles <- round(log (minTc/(cool*Tc),base=cool))
      total <- total + itry
      time2 <- round(proc.time()[3] - time1)
      time3 <-  r_cicles * max_itry * time2 / total
      conc_previous <- concluded
      concluded <- (t_cicles-r_cicles)/t_cicles
      writeLines(paste(
        paste('Temperature =',signif(Tc,digits=3)),'\n',
        paste('Rejections =',rej),'\n',
        paste('Heat exchanges =',heat),'\n',
        paste(signif(100*concluded,2),'% completed (Still missing ',r_cicles,' cycles)',sep=''),'\n',
        paste('Running for',signif(time2/3600,2),'hours (',round(time2/60),'minutes)'),'\n',
        paste('Probably still lacking',signif(time3/3600,2),'hours (',round(time3/60),'minutes)'),'\n'))
      if ( signif(1-concluded,1) < signif(1-conc_previous,1))
      {
        if (plots==T)
        {
          png(paste(location,'KimC',t_cicles-r_cicles,'T',signif(Tc,digits=2),'.png',sep=''))
          plot.network(netA,vertex.col=c,mode='circle')
          dev.off()
        }
      }
      Tc <- cool*Tc
      itry <- heat <- rej <- 0
      }
    } # end While
  # Last ouptut
  writeLines(paste(
        paste('Temperature =',signif(Tc,digits=1)),'\n',
        paste('Consecutive rejections =',rej),'\n',
        paste('Consecutive heat exchanges =',heat),'\n'))
      if (plots==T)
      {
          png(paste(location,'KimFinal','T',signif(Tc,digits=2),'.png',sep=''))
          plot.network(netA,vertex.col=c,mode='circle')
          dev.off()
          palette('default')
      }
  return(c)
}
