\name{GLOBE.ORTH}
\alias{GLOBE.ORTH}
\title{Plot globe with orthogonal
}
\description{Plot globe with orthogonal
}
\usage{
GLOBE.ORTH(lam0, phi1, R = 1, plotmap = TRUE, plotline=TRUE, add=FALSE,
 map = coastmap, mapcol = grey(0.2), linecol = grey(0.7), fill=FALSE)
}
\arguments{
  \item{lam0}{view origin longitude, degrees
}
  \item{phi1}{view origin  latitude, degrees
}
  \item{R}{Radius of sphere, default=1
}
  \item{plotmap}{logical, default=TRUE, add map
  }
  \item{plotline}{logical, default=TRUE, add grid of lat-lons
}
  \item{add}{logical, default=FALSE, Do not start a new plot, rather add to
    existing plot
}
  
  \item{map}{GEOmap list
}
  \item{mapcol}{color for map
}
  \item{linecol}{color for meridians and parallels
  }
  \item{fill}{fill polygons with color, default=FALSE
}  
}
\details{
  Plots whole globe with grid.
}
\value{Graphical Side effects
}
\references{
 Snyder, John P., Map Projections- a working manual, USGS, Professional Paper, 1987.

  
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{setPROJ, projtype, plotGEOmap
}



\examples{
######  simple map of world viewed at 40 degrees latitude
R = 1
R.MAPK = 6378.2064

phi1=40

viewlam = seq(from=0, to=340, by=2)

data(coastmap)

K=1
GLOBE.ORTH(viewlam[K], phi1, R=1, plotmap=TRUE)
######


OLIM = c(20, 40, 10, 40)
TLIM = c(-20, -10, -30, -10)


 olat = runif(1, OLIM[1], OLIM[2])
         olon = runif(1, OLIM[3], OLIM[4] )

          tlat = runif(1,TLIM[1], TLIM[2] )
         tlon = runif(1, TLIM[3], TLIM[4])

GLOBE.ORTH(olon, olat, 1,plotmap=FALSE )

 XYorg = ortho.proj(olat, olon, olon, olat, 1)
 XYtarg = ortho.proj(tlat, tlon, olon, olat, 1)

points( XYorg , col='red')
points(XYtarg , col='blue')
 da = distaz(olat, olon, tlat, tlon)
 ed2 =  Ellipsoidal.Distance(olat, olon, tlat, tlon, a=R.MAPK*1000, b=R.MAPK*1000)

  A = along.great(olat*pi/180, olon*pi/180,
seq(from=0, to=da$del, by=2)*pi/180,  da$az*pi/180)

     lat=A$phi*180/pi
     lon = A$lam*180/pi

 XYalong = ortho.proj(lat, lon, olon, olat, 1)

lines(XYalong , col='purple')

M = merid(tlon, lat1=tlat, phi1=olat, lam0=olon, R=1, by=2)

lines(M$x, M$y, col='blue' )

M2 = merid(olon, lat1=olat,  phi1=olat, lam0=olon,R=1, by=2)

lines(M2$x, M2$y, col='red' )

 leg = c( paste("del=", round(da$del)), paste("DA=", round(da$az),
 round(da$baz) ),
 paste("ED=", round(ed2$az) ,  round(ed2$revaz) ))
 
legend("topleft", legend=leg)







}
\keyword{misc}
