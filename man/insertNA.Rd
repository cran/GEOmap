\name{insertNA}
\alias{insertNA}
\title{Insert NA in a vector}
\description{
  Inserting NA values in a vector at specific index locations
}
\usage{
insertNA(y, ind)
}
\arguments{
  \item{y}{vector}
  \item{ind}{index locations where NA is inserted}
}
\details{
  The vector is parsed out and NA values are inserted where
  after the index values provided.  
}
\value{
  \item{v}{new vector with NA's}
  
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}


\examples{
  x = 1:10
 insertNA(x, 6)
}
\keyword{misc}
