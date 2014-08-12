filtro2 <- function(bd,velhoNome)
{
  novoNome <- c('Data', 'IBGEorigem', 'IBGEdestino','Especie','TotalAnimais')
  for(ii in 1:5)  {
    names(bd) <- replace(names(bd),names(bd)==velhoNome[ii], novoNome[ii]);
  }
  #Filtro por bovinos
  bd$Especie <- tolower(bd$Especie)
  bd <- bd[ grep('bov',bd$Especie), ]
  #Filtra  colunas de interesse
  bd <- bd[,c('Data','IBGEorigem','IBGEdestino','TotalAnimais')]
  return(bd)
}
