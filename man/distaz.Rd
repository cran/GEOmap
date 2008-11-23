\name{distaz}
\alias{distaz}
\title{Distance and Azimuth from two points }
\description{
  Calculate distance, Azimuth and Back-Azimuth from two points on Globe.
}
\usage{
distaz(olat, olon, tlat, tlon)
}
\arguments{
  \item{olat}{origin latitude, degrees }
  \item{olon}{origin longitude, degrees }
  \item{tlat}{target latitude, degrees }
  \item{tlon}{target longitude, degrees  }
}

\value{
  List
  \item{del}{Delta, angle in degrees}
  \item{az}{Azimuth, angle in degrees}
  \item{baz}{back Azimuth, (az+180) in degrees}
  \item{dist}{distance in km}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{along.great, getgreatarc}
\examples{

d = distaz(12, 23, -32,    -65)
d

}
\keyword{misc}
