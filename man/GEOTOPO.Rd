\name{GEOTOPO}
\alias{GEOTOPO}

\title{Topographic Plot of geographic region}
\description{
  Extract subset of a topographic database, interpolate and
  plot using the persp program.
}
\usage{
GEOTOPO(TOPO, PLOC, PROJ, calcol=NULL, npoints=500)
}

\arguments{
  \item{TOPO}{list of x,y,z for a DEM }
  \item{PLOC}{Location list, includes vectors LON and Lat}
  \item{PROJ}{projection}
  \item{calcol}{color table for coloring elevations above sea level}
   \item{npoints}{number of points in x-y grid, default=500}
}
\details{
  The return matrix PMAT is a rotation matrix used for adding
  geographic (projected) data onto the perspective plot.
}
\value{
 
  \item{PMAT}{Matrix from persp, used for adding other geographic
    information}
  \item{xo}{x-coordinates}
  \item{ yo}{y-coordinates}
   \item{IZ}{interpolated elevations}
   \item{Mollist}{matrix of RGB Colors}
   \item{dMOL}{dimensions of Mollist}

  
  
 
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{subsetTOPO,  settopocol,  subsetTOPO, persp, DOTOPOMAPI}
\examples{

library(geomapdata)


data(ETOPO5)
PLOC=list(LON=c(137.008, 141.000),LAT=c(34.000, 36.992),
            x=c(137.008, 141.000), y=c(34.000, 36.992) )

 PROJ = setPROJ(type=2, LAT0=mean(PLOC$y) , LON0=mean(PLOC$x) )
COLS = settopocol()
JMAT = GEOTOPO(ETOPO5, PLOC, PROJ, COLS$calcol)

############  this plot can be duplicated by using the output or GEOTOPO


 PMAT = persp(JMAT$xo, JMAT$yo, JMAT$IZ$z, theta = 0, phi = 90, r=4000,
col=JMAT$Mollist[1:(JMAT$dMOL[1]-1), 1:(JMAT$dMOL[2]-1)] , scale = FALSE,
      ltheta = 120, lphi=60, shade = 0.75, border = NA, expand=0.001, box = FALSE )



}

\keyword{misc}
\keyword{hplot}
