\name{projtype}
\alias{projtype}

\title{List of Projection types}
\description{
 List of Projection types in GEOMAP
}
\usage{
projtype(proj=list())
}
\arguments{
  
  \item{proj}{Projection list}

  }
\details{
 Just returns possile choices.
}
\value{
 Side Effects
}
\references{
 Snyder, John P., Map Projections- a working manual, USGS, Professional Paper, 1987.
}
\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{setPROJ}
\examples{
projtype()


proj = setPROJ(type = 1, LAT0 =23, LON0 = 35)

projtype(proj)

## or, for Kamchatka-Aleutians
LL=c(54.3861210149126,171.626386683545)

PROJ = setPROJ(type=2, LAT0=LL[1], LON0=LL[2], LATS=NULL, LONS=NULL, DLAT=NULL, DLON=NULL, FN =0)
projtype(PROJ)

}

\keyword{misc}
