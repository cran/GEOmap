\name{getETOPO}
\alias{getETOPO}
\title{Get Subset ETOPO Digital elevation map			
}
\description{Extract from ETOPO5 or ETOPO2 data
a rectangular subset of the full data.
}
\usage{
getETOPO(topo, glat = c(-90, 90), glon = c(0, 360))
}
\arguments{
  \item{topo}{A DEM matrix, ETOPO5 or ETOPO2
}
  \item{glat}{2-vector, latitude limits
}
  \item{glon}{2-vector, longitude limits (these are converted 0-360
}
}
\details{ETOPO2 and ETOPO5 are stored in a strange way:
the lons are okay the latitudes are upside down.
}
\value{returns a matrix with attributes in lat-lon that are correct
for usage in image or other R imaging programs.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{image
}
\examples{
library(geomapdata)

data(ETOPO5)

glat =c(45.4, 49)
glon = c(235, 243) 
b5 = getETOPO(ETOPO5, glat, glon)
image(x=attr(b5, 'lon'), y=attr(b5,'lat'), z=b5, col=terrain.colors(100) )
contour(   x=attr(b5, 'lon'), y=attr(b5,'lat'), z=b5, add=TRUE)

}
\keyword{misc}
