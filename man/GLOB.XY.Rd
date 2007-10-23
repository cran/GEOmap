\name{GLOB.XY}
\alias{GLOB.XY}

\title{Convert from GLOBAL LAT-LON to X-Y}
\description{
 Convert from GLOBAL LAT-LON to X-Y
}
\usage{
GLOB.XY(LAT, LON, PROJ.DATA)
}

\arguments{
  \item{LAT}{Latitude}
  \item{LON}{Longitude}
  \item{PROJ.DATA}{Projection list }
}
\details{
  Units should be given according to the projection.
  This is the inverse of  XY.GLOB.
}
\value{
 
  \item{x}{X in whatever units}
  \item{y}{Y in whatever units}

}
\references{
Snyder, John P., Map Projections- a working manual, USGS, Professional Paper, 1987.
}
\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{XY.GLOB}
\examples{


proj = setPROJ(type = 2, LAT0 =23, LON0 = 35)

### get lat-lon
LL = XY.GLOB(200, 300, proj)


##  find x-y again, should be the same
XY = GLOB.XY(LL$lat, LL$lon, proj)
XY


}

\keyword{misc}
