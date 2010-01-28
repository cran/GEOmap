\name{boundGEOmap}
\alias{boundGEOmap}

\title{Set Bounds for GEOmap}
\description{
Given a GEOmap strucutre, set the bounds for the strokes.
}
\usage{
boundGEOmap(MAP, NEGLON = FALSE, projtype = 2)
}

\arguments{
  \item{MAP}{GEOmap structure}
  \item{NEGLON}{whether to allow negative longitudes}
    \item{projtype}{suggestion (local) map projection to use when getting bounds}
}
\details{
  Used to rectify a new map after reading in from ascii
  file.  Can take GMT map ascii map files and convert to GEOmap.
}
\value{
 List structure:
  \item{STROKES}{list(nam, num, index, col, style, code, LAT1, LAT2, LON1, LON2)}
  \item{POINTS}{list(lat, lon)}
  \item{PROJ}{list(type, LAT0, LON0, LAT1, LAT2, LATS, LONS, DLAT, DLON, FE, FN, name)}
  
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{worldmap}
\examples{

library(geomapdata)
data(worldmap)
worldmap = boundGEOmap(worldmap)



}

\keyword{misc}
