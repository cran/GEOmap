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


}

\keyword{misc}
