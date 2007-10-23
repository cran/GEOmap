\name{XY.GLOB}
\alias{XY.GLOB}

\title{Convert from XY to GLOBAL LAT-LON}
\description{
 Convert from XY to GLOBAL LAT-LON
}
\usage{
XY.GLOB(x, y, PROJ.DATA)
}

\arguments{
  \item{x}{X in whatever units}
  \item{y}{Y in whatever units}
  \item{PROJ.DATA}{Projection list}
}
\details{
  Units are whatever is returned from the projection definition.
  This is the inverse of  GLOB.XY.
}
\value{

  If it is a LIST, use
  \item{lat}{Latitude}
  \item{lon}{Longitude}
  ...
}
\references{
Snyder, John P., Map Projections- a working manual, USGS, Professional Paper, 1987.

}
\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{setPROJ}
\examples{

proj = setPROJ(type = 2, LAT0 =23, LON0 = 35)

XY.GLOB(200, 300, proj)

}

\keyword{misc}
