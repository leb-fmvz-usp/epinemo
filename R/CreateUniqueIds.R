#' Create Unique Identifiers
#' @description Create unique identifiers in a network presented in edgelist form
#' @param data Edgelist of a network. \code{\link{data.frame}}
#' @param from \code{\link{character}} \code{\link{vector}}, indicate the column(s) used to characterize the origin node of each link
#' @param to \code{\link{character}} \code{\link{vector}}, indicate the column(s) used to characterize each the destiny node of each link
#' @return Edgelist with new ID's, ranging from 1:n
#' @details Used to be slow, now is very fast!
#' @references None.
#' \url{http://github.com/leb-fmvz-usp.github/epinemo}
#' @export
#' @examples 
#' # New id's
#' new_database <- CreateUniqueId(database)
CreateUniqueIds <- function(data,from,to)
{
  # Test if is there more than one identifier
  if (length(from) == 1 & length(to) == 1)
  {
    data$OriginIdOld <- data[,from]
    data$DestinyIdOld <- data[,to]
  } else
  {
    stop('Function only works with one identifier for origin and one identifier for destiny')
  }
  #Test if identifiers are numeric. If not, convert to character
  if ( !( class(data[, from]) %in% c('integer','numeric') & class(data[, to]) %in% c('integer','numeric') ))
  {
    data$OriginIdOld <- as.character(data$OriginIdOld)
    data$DestinyIdOld <- as.character(data$DestinyIdOld)
  }
  
  UniqueIdOld <- sort(unique(c(data$OriginIdOld, data$DestinyIdOld)));
  
  UniqueIdNew <- 1:length(UniqueIdOld);
  
  # Assign the unique identifier to Origins
  data <- data[order(data$OriginIdOld), ];
  freqOrigin <- table(data$OriginIdOld);
  isInOrigin <- UniqueIdOld %in% data$OriginIdOld;
  data$originID <- rep(UniqueIdNew[isInOrigin], times=freqOrigin);
  
  # Assign the unique identifier to Destinies
  data <- data[order(data$DestinyIdOld), ];
  freqDestiny <- table(data$DestinyIdOld);
  isInDestiny <- UniqueIdOld %in% data$DestinyIdOld;
  data$destinyID <- rep(UniqueIdNew[isInDestiny], times=freqDestiny);
  
  # Remove the columns used to create the unique identifier of each establishment
  data <- subset(data, select = -c(OriginIdOld, DestinyIdOld));
  
  # Create correspondence data frame
  correspondence <- data.frame(old_id = UniqueIdOld, new_id = UniqueIdNew, stringsAsFactors = F)
  
  # Create a list for output
  output <- list(movements = data, correspondence = correspondence)
  
  return(output)
}
