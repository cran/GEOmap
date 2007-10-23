\name{plotworldmap}
\alias{plotworldmap}

\title{Plot World Map with UTM sections}
\description{
Plot World Map with UTM sections
}
\usage{
plotworldmap(MAP, LIM = c(-180, -90, 180, 90), shiftlon = 0, add = TRUE, NUMB = FALSE, ...)
}

\arguments{
  \item{MAP}{GEOmap structure}
  \item{LIM}{vector of limits c(lon1, lat1, lon2, lat2) }
  \item{shiftlon}{rotate map by degrees}
  \item{add}{logical, TRUE=add to current plot}
  \item{NUMB}{logical, add numbers to plot}
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

}

\keyword{hplot}
