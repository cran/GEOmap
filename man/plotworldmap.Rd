\name{plotworldmap}
\alias{plotworldmap}

\title{Plot World Map with UTM sections}
\description{
Plot World Map with UTM sections
}
\usage{
plotworldmap(MAP, LIM = c(-180, -90, 180, 90), shiftlon = 0,
add = TRUE, NUMB = FALSE, PLOTALL=TRUE, Decorate=FALSE , ...)
}

\arguments{
  \item{MAP}{GEOmap structure}
  \item{LIM}{Vector of limits c(lon1, lat1, lon2, lat2) }
  \item{shiftlon}{Rotate map by degrees longitude (must adjust the LIM vector accordingly, see example below)}
  \item{add}{logical, TRUE=add to current plot}
  \item{NUMB}{logical, add numbers to plot}
  \item{PLOTALL}{logical, plot all strokes, do not select}
  \item{Decorate}{logical, add UTM regional designations}
  \item{\dots}{grpahical parameters from par}
}

\value{
 Graphical Side Effects
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{plotGEOmap, plotGEOmapXY}
\examples{
library(geomapdata)
data(worldmap)
plotworldmap(worldmap)
### restrict to North Atlantic:
plotworldmap(worldmap, LIM = c(0, 0, 120, 90), shiftlon=250, PLOTALL=TRUE, Decorate=FALSE )

}

\keyword{hplot}
