VerificaColunas <- function(bd1, nome)
{
  for( ii in 1:length(nome) )
  {
    aux1 <- table( nchar(bd1[, c(nome[ii]) ]) );
    print(nome[ii]);
    print(aux1);
  }
}
