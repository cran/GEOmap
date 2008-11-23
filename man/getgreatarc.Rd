\name{getgreatarc}
\alias{getgreatarc}
\title{Great Circle Arc}
\description{  Get points along great circle between two locations
}
\usage{
getgreatarc(lat1, lon1, lat2, lon2, num)
}
\arguments{
  \item{lat1}{Latitude, point 1 (degrees)}
  \item{lon1}{Longitude, point 1 (degrees) }
  \item{lat2}{Latitude, point 2 (degrees) }
  \item{lon2}{Longitude, point 2 (degrees) }
  \item{num}{number of points along arc}
}

\value{
  \item{lat}{Latitude}
  \item{lon}{Longitude}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{getgreatarc, distaz}
\examples{
PARIS = c(48.8666666666667, 2.33333333333333)
RIODEJANEIRO =c( -22.9,  -43.2333333333333)

g = getgreatarc(PARIS[1],PARIS[2], RIODEJANEIRO[1], RIODEJANEIRO[2],
100)

data(worldmap)

plotGEOmap(worldmap, add=FALSE, shiftlon=180)

lines(g$lon+180, g$lat)

}
\keyword{misc}
