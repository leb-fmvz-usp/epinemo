#' Create Unique Identifiers
#' 
#' @description Create unique identifiers in a network presented in edgelist form.
#' @param data Edgelist of a network, presented in \code{\link{data.frame}} format. Usually it is a data frame containing one row per animal movement, including information about origin and destination, number of animals, date of movement, etc.
#' @param from \code{\link{character}} \code{\link{vector}}, indicate the column(s) used to characterize the source node (origin) of each edge
#' @param to \code{\link{character}} \code{\link{vector}}, indicate the column(s) used to characterize the target node (destination) of each edge
#' @details This function creates two columns that can serve as unique identifiers. This IDs are in the range of 1:(number of nodes in the network). This is useful to create adjacency matrices when an unique identifier is composed of large numbers.
#' @return a \code{\link{list}} containing the following components: 
#' 
#' \describe{
#' \item{correspondence}{a \code{\link{data.frame}}, giving for each node its original database identifier (column named database.id) and the new unique identifier (column named network.id) created; and }
#'  
#' \item{movements}{a \code{\link{data.frame}}, which is the original dataset plus two new columns (columns "From" and "To") with the new unique identifiers of origin and destination}
#' }
#' 
#' @export
#' @examples 
#' # New id's
#' new.database <- createUniqueIds(database)
#' head(new.database$correspondence)
#' head(new.database$movements)
#' 
#' library(Matrix)
#' number.of.nodes <- max(new.database$movements$From, new.database$movements$To)
#' adjacency.matrix <- sparsematrix(i = new.database$movements$From, j=new.database$movements$To, dims = rep(number.of.nodes, 2))
#' 
createUniqueIds <- function(data, from, to)
{
  # Test if there is NA in the identifiers columns
  if ( sum( is.na( c( data[, from], data[, to]))) > 0)
    stop('NA found in one of the identifiers columns')
  
  # Test if is there more than one identifier
  if (length(from) == 1 & length(to) == 1) {
    data$from.id.old <- data[,from]
    data$to.id.old <- data[,to]
  } 
  else  {
    stop('For now, function only works with one identifier for source and one identifier for target')
  }
  
  #Test if identifiers are numeric. If not, convert to character
  if ( !( class(data[, from]) %in% c('integer','numeric') & class(data[, to]) %in% c('integer','numeric') ))
  {
    data$from.id.old <- as.character(data$from.id.old)
    data$to.id.old <- as.character(data$to.id.old)
  }
  
  unique.id.old <- sort(unique(c(data$from.id.old, data$to.id.old)));
  
  unique.id.new <- 1:length(unique.id.old);
  
  # Assign the unique identifier to Sources
  data <- data[ order( data$from.id.old), ];
  frequency.from <- table( data$from.id.old);
  is.in.from <- unique.id.old %in% data$from.id.old;
  data$From <- rep(x = unique.id.new[is.in.from], times=frequency.from);
  
  # Assign the unique identifier to Targets
  data <- data[order(data$to.id.old), ];
  frequency.to <- table(data$to.id.old);
  is.in.to <- unique.id.old %in% data$to.id.old;
  data$To <- rep(x = unique.id.new[is.in.to], times=frequency.to);
  
  # Remove the columns used to create the unique identifier of each establishment
  data <- subset(data, select = -c(from.id.old, to.id.old));
  
  # Create correspondence data frame
  correspondence <- data.frame(database.id = unique.id.old, network.id = unique.id.new, stringsAsFactors = F)
  
  # Create a list for output
  output <- list(movements = data, correspondence = correspondence)
  
  return(output)
}
