% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createUniqueIds.R
\name{createUniqueIds}
\alias{createUniqueIds}
\title{Create Unique Identifiers}
\usage{
createUniqueIds(data, from, to)
}
\arguments{
\item{data}{Edgelist of a network, presented in \code{\link{data.frame}} format. Usually it is a data frame containing one row per animal movement, including information about origin and destination, number of animals, date of movement, etc.}

\item{from}{\code{\link{character}} \code{\link{vector}}, indicate the column(s) used to characterize the source node (origin) of each edge}

\item{to}{\code{\link{character}} \code{\link{vector}}, indicate the column(s) used to characterize the target node (destination) of each edge}
}
\value{
a \code{\link{list}} containing the following components: 

\describe{
\item{correspondence}{a \code{\link{data.frame}}, giving for each node its original database identifier (column named database.id) and the new unique identifier (column named network.id) created; and }
 
\item{movements}{a \code{\link{data.frame}}, which is the original dataset plus two new columns (columns "From" and "To") with the new unique identifiers of origin and destination}
}
}
\description{
A function to create unique identifiers in a network presented in edgelist form.
}
\details{
This function creates two columns that can serve as unique identifiers. These IDs are in the range of 1:(number of nodes in the network). This is useful to create adjacency matrices when an unique identifier is composed of large numbers.
}
\examples{

# Creating a data frame with movements from a source node (origin)
# to a target node (destination) with identification codes between 1000 and 1050
origin <- sample.int(n = 50, size = 1000, replace = TRUE) + 1000
destination <- sample.int(n = 50, size = 1000, replace = TRUE) + 1000
database <- data.frame(origin = origin, destination = destination)

# Creating new IDs starting from 1
new.database <- createUniqueIds(database, from = 'origin', to = 'destination')
head(new.database$correspondence)
head(new.database$movements)

}
