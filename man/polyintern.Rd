\name{polyintern}
\alias{polyintern}

\title{Internal point of polygon}
\description{
  Find a central internal point of a polygon
}
\usage{
polyintern(P, n = 10, PLOT=FALSE)
}

\arguments{
  \item{P}{Polygon,xy}
  \item{n}{grid dimension over polygon, n by n}
  \item{PLOT}{logical, TRUE=plot}
  
}
\details{
 A grid is laid over the polygo, the internal points are extracted and
 for each one the shortest distance to te perimeter is determined.
 Then the point with the largest distance is returned.
}
\value{
 
  \item{x}{x coordinate of point}
  \item{y}{y coordinate of point}
  \item{zi}{index  of point}
  \item{nx}{internal grid points x}
  \item{ny}{internal grid points y}
  \item{ef}{internal grid points distances to perimeter}
  
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{pline}
\examples{
X=list()
X$x=c(11.991,11.942,11.891,11.834,11.775,11.725,11.691,
    11.712,11.746,11.804,11.865,11.957,11.991)
X$y=c(-2.0091,-2.0699,-2.0823,-2.1091,-2.1419,
    -2.1394,-2.1165,-2.0604,-2.0196,-1.9847,-1.9668,-1.9777,-2.0091)


polyintern(X, n = 10, PLOT=TRUE)



}

\keyword{misc}
