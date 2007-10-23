\name{GEOTOPO}
\alias{GEOTOPO}

\title{Topographic Plot of geographic region}
\description{
  Extract subset of a topographic database, interpolate and
  plot using the persp program.
}
\usage{
GEOTOPO(TOPO, PLOC, PROJ, calcol)
}

\arguments{
  \item{TOPO}{list of x,y,z for a DEM }
  \item{PLOC}{Location list, includes vectors LON and Lat}
  \item{PROJ}{projection}
  \item{calcol}{color table for coloring elevations above sea level}
}
\details{
  The return matrix PMAT is a rotation matrix used for adding
  geographic (projected) data onto the perspective plot.
}
\value{
 
  \item{PMAT}{Matrix from persp, used for adding other geographic information}
 
}

\author{Jonathan M. Lees<jonathan.lees.edu>}
\note{
}
\seealso{subsetTOPO,  settopocol,  subsetTOPO, persp, DOTOPOMAPI}
\examples{

library(geomapdata)


data(ETOPO5)
PLOC=list(LON=c(137.008, 141.000),LAT=c(34.000, 36.992),
            x=c(137.008, 141.000), y=c(34.000, 36.992) )

 PROJ = setPROJ(type=2, LAT0=mean(PLOC$y) , LON0=mean(PLOC$x) )
COLS = settopocol()
JMAT = GEOTOPO(ETOPO5, PLOC, PROJ, COLS$calcol)

}

\keyword{misc}
\keyword{hplot}
