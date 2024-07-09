\name{LLsmallcircMap}
\alias{LLsmallcircMap}
\title{World Map centered on Lat-Lon
}
\description{
  World Map centered on Lat-Lon with Lambert-Azimuthal projection.
}
\usage{
LLsmallcircMap(phicen, lamcen, worldmap, eqlat, eqlon, MAXR = 100,
circol = rgb(1, 0.8, 0.8), mapcol = rgb(0, 0, 0), eqcol = rgb(0.8, 0.8,
1) , pch=25, ecex=1 )
}
\arguments{
  \item{phicen}{Center Latitude
}
  \item{lamcen}{Center Longitude
}
  \item{worldmap}{GEOmap map structure
}
  \item{eqlat}{Latitudes of points, vector
}
  \item{eqlon}{Longitude of points, vector
}
  \item{MAXR}{Maximum radius, degrees
}
  \item{circol}{Color for small circles
}
  
  \item{mapcol}{Color for map
  }
  \item{eqcol}{Color for points, single or 2-vector
  }
   \item{pch}{Plotting character for points
  }
   \item{ecex}{Plotting size for points
  }
  
}
\details{
  Uses a Lamber-Azimuthal projection of the whole globe
  out to the given radius.  If a vector of 2 colors are provided for the
  eqcol parameter, and the pch is one of (21:25), then a 2-tone
  points is plotted with ecol[1] on the perimeter, and ecol[2] on the
  interior.
  
}
\value{
  Graphical side effects 
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{lamaz.eqarea
}
\examples{
###  earthquake in Noto,Japan

data(coastmap)


elat = 37.4949
elon = 137.2653

K = list(lon=c(183.3158,188.2173,253.5428,295.3037,
               166.4531,110.5354,268.7554,98.9443,212.1384,236.6954),
         lat=c( 51.8823,-13.9085,34.94591,32.3713,68.0653,
               -66.2792,38.0557,18.8141,64.8736,44.5855  ))



LLsmallcircMap(elat, elon, coastmap, K$lat, K$lon  )

LLsmallcircMap(elat, elon, coastmap, K$lat, K$lon,
  MAXR=80, eqcol=c('blue', 'gold') , mapcol=grey(.8), pch=22, ecex=1.5  )





}
\keyword{misc}
