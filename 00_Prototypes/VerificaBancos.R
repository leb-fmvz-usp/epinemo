VerificaBancos <- function(){
  estados <- c("AC", "AL", "AM", "BA", "CE", "DF", "ES", "GO", "MG", "MS", "MT", "PA", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO")
  
  for( ii in 1: length(estados) ){
    print(estados[ii]);
    bdTemp <- read.csv2(file=paste( "BD_", toupper(estados[ii]), ".csv", sep="" ), as.is=TRUE);
    print( nrow(bdTemp) );
    
    aux1 <- as.numeric( is.na( as.numeric(bdTemp$IBGEorigem) ) );
    aux2 <- as.numeric( is.na( as.numeric(bdTemp$IBGEdestino) ) );
    aux3 <- as.numeric( is.na( as.numeric(bdTemp$TotalAnimais) ) );
    aux4 <- (aux1+aux2+aux3)>0;
    bdTemp <- bdTemp[ aux4==FALSE, ];
    print( nrow(bdTemp) );
  }
}
