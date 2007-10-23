\name{getGEOperim}
\alias{getGEOperim}

\title{Get Lat-Lon Perimeter}
\description{
Get rectangular perimeter of region defined by set of Lat-Lon
}
\usage{
getGEOperim(lon, lat, PROJ, N)
}

\arguments{
  \item{lon}{vector of lons}
  \item{lat}{vector of lats}
  \item{PROJ}{projection structure}
  \item{N}{number of points per side}
}
\details{
 perimeter is used for antipolygon
}
\value{
  List:
  \item{x}{x-coordinates projected}
  \item{y}{y-coordinates projected}
  
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\examples{

###  target region
PLOC= list(LON=c(138.3152, 139.0214),
LAT=c(35.09047, 35.57324))

PLOC$x =PLOC$LON
PLOC$y =PLOC$LAT

####  set up projection
PROJ = setPROJ(type=2, LAT0=mean(PLOC$y) , LON0=mean(PLOC$x) )



perim= getGEOperim(PLOC$LON, PLOC$LAT, PROJ, 50)

}

\keyword{misc}
