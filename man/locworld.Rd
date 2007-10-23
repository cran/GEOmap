\name{locworld}
\alias{locworld}

\title{Locate points in worlmap}
\description{
  Locate points in worlmap
}
\usage{
locworld(shiftlon = 0, col = "brown", n = 2)
}

\arguments{
  \item{shiftlon}{rotate map by degrees}
  \item{col}{color of points}
  \item{n}{number of points}
}

\value{
  

  \item{lon}{longitudes}
  \item{lat}{latitudes}
  \item{LON}{longitudes}
  \item{LAT}{latitudes}
  \item{utmbox}{UTM box list(lat, lon) }
  \item{x}{UTM x-coordinates}
  \item{y}{UTM y-coordinates}
  \item{UTM0}{utm origin for projection list(phi, lam) }
  \item{shiftlon}{rotate map by degrees}
   
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{plotworldmap}
\examples{

###  this program is interactive....
\dontrun{

library(geomapdata)

data(worldmap)
plotworldmap(worldmap)
locworld(shiftlon = 0, col = "brown", n = 2)
}

}

\keyword{iplot}
