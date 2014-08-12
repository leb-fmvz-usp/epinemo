SeparaColuna <- function(coluna, sep="-")
{
  colunaSeparada <- strsplit(coluna, sep);
  
  quantidadeDeColunas <- max( as.numeric( names( table(sapply(colunaSeparada, FUN=length)) ) ) );
  if(quantidadeDeColunas>2){
    aux1 <- which(sapply(colunaSeparada, FUN=length)>2);
    coluna[aux1] <- gsub("(.*)\\-", "\\1 ", coluna[aux1]);
    colunaSeparada <- strsplit(coluna, "-");
  }
    
  DFcolunaSeparada <- do.call(rbind, colunaSeparada);
  DFcolunaSeparada[,1] <- gsub(pattern=" ", replacement="", x=DFcolunaSeparada[,1]);
  
  return(DFcolunaSeparada[,1]);
}
