\name{GEOTOPO}
\alias{GEOTOPO}

\title{Topographic Plot of geographic region}
\description{
  Extract subset of a topographic database, interpolate and
  plot using the persp program.
}
\usage{
GEOTOPO(TOPO, PLOC, PROJ, calcol=NULL, nx=500, ny=500, nb = 4, mb = 4, hb = 8, PLOT=TRUE)
}

\arguments{
  \item{TOPO}{list of x,y,z for a DEM }
  \item{PLOC}{Location list, includes vectors LON and Lat}
  \item{PROJ}{projection}
  \item{calcol}{color table for coloring elevations above sea level}
  \item{nx}{number of points in x grid, default=500}
  \item{ny}{number of points in y grid, default=500}
  \item{nb}{see function mba.surf, default = 4}
   \item{mb}{see function mba.surf, default = 4}
   \item{hb}{see function mba.surf , default= 8}
  \item{PLOT}{ logical, TRUE=plot a map and return color map}
}
\details{
  The return matrix PMAT is a rotation matrix used for adding
  geographic (projected) data onto the perspective plot.

   ETOPO5 or ETOPO2 can be downloaded from
  and installed using these links:
  \url{http://leesj.sites.oasis.unc.edu/FETCH/GRAB/RPACKAGES/ETOPO2.RData}
  and
  \url{http://leesj.sites.oasis.unc.edu/FETCH/GRAB/RPACKAGES/ETOPO5.RData}

  
}
\value{
 
  \item{PMAT}{Matrix from persp, used for adding other geographic
    information}
  \item{xo}{x-coordinates}
  \item{ yo}{y-coordinates}
   \item{IZ}{interpolated elevations}
   \item{Cmat}{matrix of RGB Colors}
   \item{Dcol}{dimensions of Cmat}

  
  
 
}
\note{
  If PLOT is false the transform matrix PMAT and the color
  mapping matrix Cmat will be returned as NA.
  To create these for future plotting, use TOPOCOL or LandSeaCol
  functions.  TOPOCOL simply assigns values above sea level with
  one color scale and those below with under water colors.
  LandSeaCol requires a coastal map and fills in
  land areas with terrain colors and sea areas with blue palette colors.
  
}



\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{subsetTOPO, TOPOCOL,  LandSeaCol, settopocol,  subsetTOPO, persp, DOTOPOMAPI}
\examples{


\dontrun{

library(geomapdata)

####  need to download and install ETOPO data
###   data(ETOPO5)
load(ETOPO5)
PLOC=list(LON=c(137.008, 141.000),LAT=c(34.000, 36.992),
            x=c(137.008, 141.000), y=c(34.000, 36.992) )

 PROJ = setPROJ(type=2, LAT0=mean(PLOC$y) , LON0=mean(PLOC$x) )
COLS = settopocol()
JMAT = GEOTOPO(ETOPO5, PLOC, PROJ, COLS$calcol, nx=1000, ny=1000, nb=8, mb=8, hb=12, PLOT=TRUE)

############  this plot can be duplicated by using the output or GEOTOPO


 PMAT = persp(JMAT$xo, JMAT$yo, JMAT$IZ$z, theta = 0, phi = 90, r=4000,
col=JMAT$Cmat[1:(JMAT$Dcol[1]-1), 1:(JMAT$Dcol[2]-1)] , scale = FALSE,
      ltheta = 120, lphi=60, shade = 0.75, border = NA, expand=0.001, box = FALSE )


}


}

\keyword{misc}
\keyword{hplot}
