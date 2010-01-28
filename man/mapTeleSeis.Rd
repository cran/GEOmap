\name{mapTeleSeis}
\alias{mapTeleSeis}
\title{World Map with Teleseismic Ray-paths
}
\description{World Map with Teleseismic Ray-paths
}
\usage{
mapTeleSeis(sta, mylist)
}
\arguments{
  \item{sta}{list of station locations
}
  \item{mylist}{list of event locations
}
}
\details{Uses GEOmap. No projection is used.
}
\value{Graphical side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{

\dontrun{

################### 
######   get stations
load("/Users/lees/SANTIAGUITO/SGRobjs/GUAT09.sta.RData")
sta$name = sta$nam

##############   get earthquake epicenters
eq1 = scan(file="sg06_eqs_neic.csv", skip=1, sep=",", what=list(yr=0, mo=0, dom=0, t1="", lat=0, lon=0, mag=0, depth=0))

######  parse out the time:
eq1$hr = as.numeric( substr(eq1$t1, 1,2))
eq1$mi = as.numeric( substr(eq1$t1, 3,4))
eq1$sec = as.numeric( substr(eq1$t1, 5,9))

eq1$z = eq1$depth
eq1$jd = getjul(eq1$yr, eq1$mo,  eq1$dom)

##########################  use the projection that is derived from the 
##########################    station file - these are based on the median station locations
proj =  stinfo$proj

######   get distances - this is so we can separate regional from teleseismic events
eqdists = distaz(stinfo$mlat , stinfo$mlon, eq1$lat,  eq1$lon)

######  110 degrees = 1 degree is an arbitrary cut off

w1 = which(eqdists$dist<110)

##########   restrict the global data to only very large events?
w2 = which(eq1$mag>5.5)


##    create a an earthquake structure list

W = c(w1, w2)
##  W = c(w1 )


mylist = list()
for(i in 1:length(W))
{
j = W[i] 
mylist[[i]] = list(yr=eq1$yr[j], jd=eq1$jd[j], mo=eq1$mo[j], dom=eq1$dom[j], hr=eq1$hr[j], 
mi=eq1$mi[j], sec=eq1$sec[j], lat=eq1$lat[j], lon=eq1$lon[j], z=eq1$z[j], mag=eq1$mag[j])


}

library(GEOmap)
library(geomapdata)
     data(worldmap)
 mapTeleSeis(sta, mylist)



}





}
\keyword{misc}
