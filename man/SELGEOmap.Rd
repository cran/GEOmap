\name{SELGEOmap}
\alias{SELGEOmap}

\title{Select parts of a GEOmap}
\description{
  Using area, number of points and Lat-Lon Limits, extracts
  map strokes and creates a new GEOmap
}
\usage{
SELGEOmap(MAP, ncut = 3, acut = c(0, 1e+05), proj = NULL, LIM = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MAP}{Map structure}
  \item{ncut}{minimum number of points in polygon}
  \item{acut}{vector, min and max of areas to include}
  \item{proj}{map projection}
  \item{LIM}{vector, c(lon1, lat1, lon2, lat2)  }
}
\details{
  Uses sf::st_area  function.  If proj and LIM are NULL then
  no selection on limits are used
  ncut is used to eliminate area calculations with strokes less than the
  specified number.
}
\value{
  GEOmap LIST
\item{STROKES}{list}
   \item{nam}{name of stroke}
   \item{num}{number of points in stroke}
   \item{index}{index of stroke}
   \item{col}{color of stroke}
   \item{style}{style of stroke}
   \item{code}{code of stroke}
   \item{LAT1}{lower left Lat of stroke}
   \item{LAT2}{upper right Lat  of stroke}
   \item{LON1}{lower left Lon  of stroke}
   \item{LON2}{upper right Lon of stroke}
   \item{POINTS}{list}
   \item{lat}{vector of lats}
   \item{lon}{vector of lons}

}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{geoarea, sf::st_area}
\examples{

library(geomapdata)
data(worldmap)
skam = SELGEOmap(worldmap, ncut=3, acut=c(10000, Inf), proj=NULL, LIM=NULL)

par(mfrow=c(2,1))

#######  plot world map, with all lines:
plotGEOmap(worldmap)
length(worldmap$STROKES$num)
######   same plot with some lines removed:
plotGEOmap(skam)
length(skam$STROKES$num)

#####################
#####################

}

\keyword{misc}
