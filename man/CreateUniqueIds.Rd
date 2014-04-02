\name{CreateUniqueIds}
\alias{CreateUniqueIds}
\title{Create Unique Identifiers}
\usage{
CreateUniqueIds(data, from, to)
}
\arguments{
  \item{data}{Edgelist of a network.
  \code{\link{data.frame}}}

  \item{from}{\code{\link{character}} \code{\link{vector}},
  indicate the column(s) used to characterize the origin
  node of each link}

  \item{to}{\code{\link{character}} \code{\link{vector}},
  indicate the column(s) used to characterize each the
  destiny node of each link}
}
\value{
Edgelist with new ID's, ranging from 1:n
}
\description{
Create unique identifiers in a network presented in
edgelist form
}
\details{
Used to be slow, now is very fast!
}
\examples{
# New id's
new_database <- CreateUniqueId(database)
}
\references{
None. \url{http://github.com/leb-fmvz-usp.github/epinemo}
}

