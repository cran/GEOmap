\name{along.great}
\alias{along.great}
\title{Along A great Arc}
\description{Calculate points along a great arc
}
\usage{
along.great(phi1, lam0, c, Az)
}
\arguments{
  \item{phi1}{start lat, radians}
  \item{lam0}{start lon, radians}
  \item{c}{distance, radians}
  \item{Az}{Azimuthal direction, radiansm  }
}
\details{All input and output is radians
}
\value{
  List:
  \item{phi}{latitudes, radians}
  \item{lam}{longitudes, radians}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\examples{
lat1 <- 48.856578
lon1 <- 2.351828	

A = along.great(lat1*pi/180, lon1*pi/180, 50*pi/180, -63*pi/180)

lat=A$phi*180/pi
lon = A$lam*180/pi


}
\keyword{misc}
