\name{subsetTOPO}
\alias{subsetTOPO}

\title{Subset a Topo map }
\description{
 Extract a subset of a topo DEM 
}
\usage{
subsetTOPO(TOPO, ALOC)
}

\arguments{
  \item{TOPO}{DEM list including x,y,z}
  \item{ALOC}{list including  LAT LON vectors for extracting an array
    from the DEM}
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
library(geomapdata)
data(ETOPO5)
PLOC=list(LON=c(137.008, 141.000),LAT=c(34.000, 36.992),
            x=c(137.008, 141.000), y=c(34.000, 36.992) )


JAPANtopo = subsetTOPO(ETOPO5, PLOC)

}

\keyword{misc}
