\name{ortho.proj}
\alias{ortho.proj}
\title{Orthogonal Map Projection
}
\description{Orthogonal Map Projection
}
\usage{
ortho.proj(lat, lon, lon0, lat1, R)
}
\arguments{
  \item{lat}{latitude, degrees
}
  \item{lon}{longitude, degrees
}
  \item{lon0}{view origin longitude, degrees
}
  \item{lat1}{view origin  latitude, degrees
}
  \item{R}{Radius of sphere, default=1
}
}
\details{
  Assumes spherical globe.  This function is not part
  of the normal GEOmap plotting routines.
}
\value{
  list
  \item{x}{x, coordinate in units of R}
  \item{y}{y, coordinate in units of R}
}
\references{
  Snyder, John P., Map Projections- a working manual, USGS, Professional Paper, 1987.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{GLOBE.ORTH, setPROJ, projtype
}
\examples{


olat = 0
         olon = 0

          tlat = 23
         tlon = 30
R = 1
ortho.proj(tlat, tlon, olon, olat, R)


}
\keyword{misc}
