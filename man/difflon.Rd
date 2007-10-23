\name{difflon}
\alias{difflon}

\title{Difference between Longitudes}
\description{
 Difference between Longitudes
}
\usage{
difflon(LON1, LON2)
}

\arguments{
  \item{LON1}{Longitude in degrees}
  \item{LON2}{Longitude in degrees}
}
\details{
 takes into account crossing the zero longitude
}
\value{
  \item{deg}{degrees difference}
  \item{sn}{direction of rotation}
    
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\examples{

difflon( 34 , 67)

###  here we cross the zero line
difflon( 344 , 67)

}

\keyword{misc}
