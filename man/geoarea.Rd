\name{geoarea}
\alias{geoarea}

\title{Area of Map objects}
\description{
  vector of areas of polygons in map
}
\usage{
geoarea(MAP, proj=NULL, ncut=10)
}

\arguments{
  \item{MAP}{Map structure}
  \item{proj}{projection}
  \item{ncut}{minimum number of points in polygon}
  
}
\details{
  Uses sf function.  If proj is NULL then the project is
  reset to UTM spherical for each element seperately to calculate the
  area in km.
  ncut is used to eliminate area calculations with strokes less than the
  specified number.
}
\value{
 vector of areas
}
\seealso{sf::st_area}
\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
  areas smaller than a certain tolerance are NA
}


\keyword{misc}
