\name{maplim}
\alias{maplim}
\title{Map Limits
}
\description{Set reasonable map limits from
  a set of Lat-Lon pairs.
}
\usage{
maplim(lat, lon, pct = 0.1)
}
\arguments{
  \item{lat}{vector of latitudes
}
  \item{lon}{vector of longitudes
}
  \item{pct}{percent fraction to increase (or decrease) limits
}
}
\details{
  In some (GEOmap) programs the longitude
  needs to be modulus 360, so these are provided also.
  
}
\value{
  list of range of lats and lons
  \item{lat}{lat limits}
  \item{lon}{lat limits}
  \item{LON}{lon limits modulus 360}
  \item{lim}{ vector: lon1 lat1 lon2 lat2}
  \item{LIM}{ vector: lon1 lat1 lon2 lat2, with  lon limits modulus 360}

}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{expandbound, plotGEOmapXY
}
\examples{

lat = rnorm(10, m=46, sd=2)
lon = rnorm(10, m=-121, sd=1)

M = maplim(lat, lon, pct=.2)

plot(M$lon, M$lat, type='n')
points(lon, lat)

############   plotting with a GEOmap
library(geomapdata)
data(worldmap)

PROJ = setPROJ(type=2, LON0=mean(lon), LAT0=mean(lat))

plotGEOmapXY(worldmap, LIM=M$LIM)
pointsGEOmapXY(lat, lon,PROJ =PROJ,  pch=6)




}
\keyword{misc}
