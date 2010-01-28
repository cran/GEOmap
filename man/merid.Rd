\name{merid}
\alias{merid}
\alias{paral}

\title{Orthogonal Projection of  Meridian or Parallel
}
\description{Orthogonal Projection Meridian or Parallel 
}
\usage{
merid(lon, lat1=-90,   lat2=90,  lam0=0, phi1=41, R=1, by=1)
paral(lat, lon1=-180 , lon2=180, lam0=0, phi1=41, R=1, by=1)
}
\arguments{
  \item{lon}{merid starting Longitude, degrees
}
  \item{lat}{paral starting Latitude, degrees
}
  \item{lam0}{origin Longitude, degrees
}
\item{phi1}{origin  Latitude, degrees
}
  \item{R}{Radius
  }
 \item{by}{increment in degrees
  }

  \item{lat1}{merid starting Latitude, degrees
}
  \item{lat2}{merid ending Latitude, degrees
}

  \item{lon1}{paral starting Longitude, degrees
}
  \item{lon2}{paral ending Longitude, degrees
}


  
}
\details{
  Retruns points along a meridian running through lat, lon
  with a projection based on lam0 phi.

  
}
\value{
  list of x-y values for plotting
}
\references{
  Snyder, John P., Map Projections- a working manual, USGS, Professional Paper, 1987.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{ortho.proj
}
\examples{

 olat = 0
         olon = 0

          tlat = 23
         tlon = 30

M = merid(tlon, lat1=tlat, olon, olat, 1)


R = 1

phi1=40


GLOBE.ORTH(20, phi1, 1,plotmap=FALSE)

M1 = merid(20, lat1=20, lat2=40, phi1=phi1, lam0=olat, R=1, by=1)

P2 = paral(40, lon1=20 , lon2=40, lam0=olat, phi1=phi1, R=1, by=1)

M2 = merid(40, lat1=40, lat2=20, phi1=phi1, lam0=olat, R=1, by=1)

P1 = paral(20, lon1=40 , lon2=20, lam0=olat, phi1=phi1, R=1, by=1)

polygon(c(M1$x, P2$x, M2$x, P1$x), c(M1$y, P2$y, M2$y, P1$y),
col=rgb(.8, .8, 1))


}
\keyword{misc}
