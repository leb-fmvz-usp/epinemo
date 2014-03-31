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
#' @references Kim (2010), (Kirkpatrick, Gelatt e Vecchi, 1983)
#' 
#' \url{http://github.com/leb-fmvz-usp.github/epinemo}
#' @export
#' @examples 
#' # Calculates the optimal partition of a given network
#' LinkRankOptimalPartition(A)

LinkRankOptimalPartition <- function(qlrM,A,c,Tc=1,minTc=1e-10,cool=0.995,max_itry,max_heat,max_rej,plots=F,location=getwd())
{
#   Parâmetros
  if (missing(qlrM))
  {
    pr <- PageRank(A)
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
#   Variáveis iniciais
  itry <- total <- heat <- rej <- concluded <- 0
  t_cicles <- round(log (minTc/Tc,base=cool))
  time1 <- proc.time()[3]
  while (Tc >= minTc & rej < max_rej)
  {
    itry <- itry+1
    #Sorteia um nó e uma comunidade para trocar
    nsort <- sample(n,1)
    csort <- sample(lc[-which(lc==c[nsort])],1)
    # daí calcula a contribuição do nó 'n' quando em uma comunidade, e quando em outra
    LqlrM <- qlrM[nsort,]
    CqlrM <- qlrM[,nsort]
    oldm <- sum(LqlrM[c==c[nsort]]) + sum(CqlrM[c==c[nsort]]) #contribuição atual do nó sorteado na modularidade
    newm <- sum(LqlrM[c==csort]) + sum(CqlrM[c==csort]) #contribuição futura do nó sorteado na modularidade
    # e agora decide se troca ou não
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
        paste('Temperatura =',signif(Tc,digits=3)),'\n',
        paste('Rejeições =',rej),'\n',
        paste('Trocas por calor =',heat),'\n',
        paste(signif(100*concluded,2),'% concluído (Faltam ',r_cicles,' ciclos)',sep=''),'\n',
        paste('Estou rodando há',signif(time2/3600,2),'horas (',round(time2/60),'minutos)'),'\n',
        paste('Acho que faltam',signif(time3/3600,2),'horas (',round(time3/60),'minutos)'),'\n'))
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
    } #end While
  #Last ouptut
  writeLines(paste(
        paste('Temperatura =',signif(Tc,digits=1)),'\n',
        paste('Rejeições consecutivas =',rej),'\n',
        paste('Trocas por calor consecutivas =',heat),'\n'))
      if (plots==T)
      {
          png(paste(location,'KimFinal','T',signif(Tc,digits=2),'.png',sep=''))
          plot.network(netA,vertex.col=c,mode='circle')
          dev.off()
          palette('default')
      }
  return(c)
}
