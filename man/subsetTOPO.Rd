\name{subsetTOPO}
\alias{subsetTOPO}

\title{Subset a Topo map }
\description{
 Extract a subset of a topo DEM 
}
\usage{
subsetTOPO(TOPO, ALOC, PROJ, nx=500, ny=500, nb = 4, mb = 4, hb = 8)
}

\arguments{
  \item{TOPO}{DEM list including x,y,z}
  \item{ALOC}{list including  LAT LON vectors for extracting an array
    from the DEM}
   \item{PROJ}{projection}
   \item{nx}{number of points in x grid, default=500}
  \item{ny}{number of points in y grid, default=500}
  \item{nb}{see function mba.surf, default = 4}
   \item{mb}{see function mba.surf, default = 4}
   \item{hb}{see function mba.surf , default= 8}
}
\details{
  Used for extracting a subset of ETOPO5 or ETOPO2
}
\value{
 
  \item{x}{vector x-coordinates}
  \item{y}{vector y-coordinates}
  \item{z}{2D matrix of elevations}

}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{GEOTOPO}
\examples{
\dontrun{
library(geomapdata)
data(ETOPO5)
PLOC=list(LON=c(137.008, 141.000),LAT=c(34.000, 36.992),
            x=c(137.008, 141.000), y=c(34.000, 36.992) )

 PROJ = setPROJ(type=2, LAT0=mean(PLOC$y) , LON0=mean(PLOC$x) )
JAPANtopo = subsetTOPO(ETOPO5, PLOC, PROJ)
}


}

\keyword{misc}
