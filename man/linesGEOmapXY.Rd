\name{linesGEOmapXY}
\alias{linesGEOmapXY}
\alias{pointsGEOmapXY}
\alias{textGEOmapXY}

\title{Add lines, points or text to GEOmap projected plot}
\description{
  Add lines, points or text to GEOmap projected plot
  
}
\usage{
linesGEOmapXY(lat = 0, lon = 0, PROJ = NULL, PMAT = NULL, ...)
textGEOmapXY(lat = 0, lon = 0, labels = NULL, PROJ = NULL, PMAT = NULL, ...)
pointsGEOmapXY(lat = 0, lon = 0, PROJ = NULL, PMAT = NULL, ...)


}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{vector of latitudes}
  \item{lon}{vector of longitudes}
  \item{labels}{text for labels}
  \item{PROJ}{projection structure}
  \item{PMAT}{persp matrix for perspective plot}
  
  \item{\dots}{graphical Parameters from par}
}

\value{
 Graphical Side Effects
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{plotGEOmapXY}

\keyword{aplot}
