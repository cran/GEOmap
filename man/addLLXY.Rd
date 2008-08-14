\name{addLLXY}
\alias{addLLXY}

\title{Add Lat-Lon points using projection}
\description{
Add Lat-Lon points using projection
}
\usage{
addLLXY(lats, lons, PROJ = PROJ, PMAT = NULL, col = gray(0.7), GRID = TRUE, GRIDcol = 1, LABS = NULL, LABcol = 1, BORDER = NULL, TICS = c(1, 1), xpd=TRUE)
}

\arguments{
  \item{lats}{Latitudes in Degrees}
  \item{lons}{Longitude in  Degrees}
  \item{PROJ}{Map Projection list}
  \item{PMAT}{Perspective matrix conversion}
  \item{col}{color}
  \item{GRID}{logical, TRUE=add grid lines}
  \item{GRIDcol}{color for grid lines}
  \item{LABS}{vector of labels}
  \item{LABcol}{color for labels}
  \item{BORDER}{add border}
  \item{TICS}{tick marks}
    \item{xpd}{logical, expand plotting region (see par)}
}

\value{
 Graphical Side Effects
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{plotGEOmapXY, sqrTICXY}
\examples{

library(geomapdata)


data(fujitopo)
data(japmap)

PLOC=list(LON=range(c( japmap$STROKES$LON1,japmap$STROKES$LON2) ),
LAT=range(c( japmap$STROKES$LAT1,japmap$STROKES$LAT2) ))
PLOC$x = PLOC$LON
PLOC$y = PLOC$LAT

PROJ = setPROJ(type=2, LAT0=mean(PLOC$y) , LON0=mean(PLOC$x) )

plotGEOmapXY(japmap, PROJ=PROJ, add=FALSE, axes=FALSE, xlab="", ylab="")
A = PLOC

   PLAT =  pretty(A$LAT)
    PLAT = c(min(A$LAT),  PLAT[PLAT>min(A$LAT) & PLAT<max(A$LAT)],max(A$LAT)) 
  PLON  = pretty(A$LON)
        PLON = c(min(A$LON), PLON[PLON>min(A$LON) & PLON<max(A$LON)], max(A$LON))


addLLXY(PLAT,  PLON, PROJ=PROJ, LABS=TRUE, PMAT=NULL, TICS=c(.1,.1) )

###############


}

\keyword{aplot}
