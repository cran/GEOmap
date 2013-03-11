\name{plotUTM}
\alias{plotUTM}

\title{Plot UTM }
\description{
  Plot UTM 
}
\usage{
plotUTM(proj, LIM, shiftlon = 0)
}

\arguments{
  \item{proj}{projection}
  \item{LIM}{Limit vector}
  \item{shiftlon}{rotation around z axiz, default=0}
}

\value{
 Graphical Side Effect
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{GLOB.XY}
\examples{
library(geomapdata)

data(USAmap)


proj = setPROJ(type=3, LAT0=33.75, LON0= RPMG::fmod(-79., 360) ,
  LAT1=34.333333,  LAT2=36.166667, LATS=NULL, LONS=NULL,
   DLAT=NULL, DLON=NULL,FE=0,FN=0)



ALOC=list(lon=c(274.5,288), lat=c(31, 38),
         LON=c(274.5, 288), LAT=c(31, 38), shiftlon=0)

plotGEOmapXY(USAmap, LIM=c(ALOC$LON[1], ALOC$lat[1],
     ALOC$LON[2], ALOC$lat[2]) , PROJ=proj, add=FALSE, shiftlon=0)


plotUTM(proj, c(ALOC$LON[1], ALOC$lat[1], ALOC$LON[2], ALOC$lat[2]))

##############  larger scale

\dontrun{
p = plotusa()

plotUTM(p$PROJ, LIM=p$LIM)
}

}


\keyword{misc}

