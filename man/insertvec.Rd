\name{insertvec}
\alias{insertvec}
\title{Insert a set of values in a vector}

\description{
  Inserting values in a vector at specific index locations
}

\usage{
insertvec(v, ind, val)
}

\arguments{
  \item{v}{vector}
  \item{ind}{ndex locations where val is inserted}
  \item{val}{some vector of insertion, maybe NA}
}
\details{
 The vector is parsed out and val values are inserted where
  after the index values provided.   
}
\value{
   \item{v}{new vector with val inserted after the index}
 }
\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

 
\examples{
  x = 1:20

 insertvec(x, c(4,17) , NA)
}
\keyword{misc}
