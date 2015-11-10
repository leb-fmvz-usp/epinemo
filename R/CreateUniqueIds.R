#' Create Unique Identifiers
#' @description Create unique identifiers in a network presented in edgelist form
#' @param data Edgelist of a network. \code{\link{data.frame}}
#' @param from \code{\link{character}} \code{\link{vector}}, indicate the column(s) used to characterize the source node of each link
#' @param to \code{\link{character}} \code{\link{vector}}, indicate the column(s) used to characterize each the target node of each link
#' @return \code{\link{list}} containing 2 elements: a \code{\link{data.frame}}
#'          with database and network IDs for each node; and the original dataset with
#'          the new IDs, ranging from 1:n (columns "From" and "To")
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
    data$FromIdOld <- data[,from]
    data$ToIdOld <- data[,to]
  } else
  {
    stop('Function only works with one identifier for source and one identifier for target')
  }
  #Test if identifiers are numeric. If not, convert to character
  if ( !( class(data[, from]) %in% c('integer','numeric') & class(data[, to]) %in% c('integer','numeric') ))
  {
    data$FromIdOld <- as.character(data$FromIdOld)
    data$ToIdOld <- as.character(data$ToIdOld)
  }
  
  UniqueIdOld <- sort(unique(c(data$FromIdOld, data$ToIdOld)));
  
  UniqueIdNew <- 1:length(UniqueIdOld);
  
  # Assign the unique identifier to Sources
  data <- data[order(data$FromIdOld), ];
  freqFrom <- table(data$FromIdOld);
  isInFrom <- UniqueIdOld %in% data$FromIdOld;
  data$From <- rep(UniqueIdNew[isInFrom], times=freqFrom);
  
  # Assign the unique identifier to Destinies
  data <- data[order(data$ToIdOld), ];
  freqTo <- table(data$ToIdOld);
  isInTo <- UniqueIdOld %in% data$ToIdOld;
  data$To <- rep(UniqueIdNew[isInTo], times=freqTo);
  
  # Remove the columns used to create the unique identifier of each establishment
  data <- subset(data, select = -c(FromIdOld, ToIdOld));
  
  # Create correspondence data frame
  correspondence <- data.frame(database_id = UniqueIdOld, network_id = UniqueIdNew, stringsAsFactors = F)
  
  # Create a list for output
  output <- list(movements = data, correspondence = correspondence)
  
  return(output)
}
