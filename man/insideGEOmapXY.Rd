\name{insideGEOmapXY}
\alias{insideGEOmapXY}

\title{Get LAT-LON points that fall inside a map}
\description{
  Get LAT-LON points that fall inside a map
}
\usage{
insideGEOmapXY(lat, lon, PROJ = NULL, R = NULL, PMAT = NULL)
}

\arguments{
  \item{lat}{vector of latitudes}
  \item{lon}{vector of longitudes}
  \item{PROJ}{projection structure}
  \item{PMAT}{persp matrix for perspective plot}

  \item{R}{List(lat, lon, radius) for selecting instead of using usr coordinates}

}
\details{
  The parameters par("usr") is queried and used to select the
  lat and lons that fall within the mapped region.  If the list
  R=list(lat, lon, radius) is provided, then all indeces of points falling within
  that radius are returned.
}
\value{
Vector of index values for points that satisfy geographic criteria
}

\author{Jonathan M. Lees<jonathan.lees.edu>}


\examples{

\dontrun{
library(geomapdata)

data(japmap)
 PROJfuji = setPROJ(type = 2, LAT0=35.358,LON0=138.731)
plotGEOmapXY(japmap, PROJ=PROJfuji, add=FALSE)
pointsGEOmapXY(gvol$lat, gvol$lon,  PROJ=PROJfuji)
textGEOmapXY(gvol$lat, gvol$lon, gvol$name,  PROJ=PROJfuji, pos=4,
cex=.5)
wv =insideGEOmapXY(gvol$lat, gvol$lon, PROJfuji)
cbind(gvol$name[wv], gvol$lat[wv], gvol$lon[wv])


}




}

\keyword{misc}
