\name{addTIX}
\alias{addTIX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Add Tic marks to map}
\description{
  Add Tic marks to map
}
\usage{
addTIX(lats, lons, PROJ = list(), PMAT = NULL,
col = gray(0.7), TICS = c(1, 1), OUTER = TRUE,
sides = c(1, 2, 3, 4))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lats}{Latitudes in Degrees}
  \item{lons}{Longitude in  Degrees}
  \item{PROJ}{Map Projection list }
  \item{PMAT}{Perspective matrix conversion}
  \item{col}{color}
  \item{TICS}{tic labels}
  \item{OUTER}{logical}
  \item{sides}{sides, 1,2,3,4}
}
\details{
 attempts to make correct default values
}
\value{
  Graphical Side Effects

}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{addLLXY}
\examples{

##########3  this program is run internally


PLOC=list(LON=c(137.008, 141.000),
LAT=c(34.000, 36.992),
x=c(137.008, 141.000),
y=c(34.000, 36.992))

PROJ = setPROJ(type=2, LAT0=mean(PLOC$y) , LON0=mean(PLOC$x) )

gxy = GLOB.XY(PLOC$LAT, PLOC$LON, PROJ)

PLAT =  pretty(PLOC$LAT)

PLAT = c(min(PLOC$LAT),PLAT[PLAT>min(PLOC$LAT)&PLAT<max(PLOC$LAT)],max(PLOC$LAT)) 

PLON  = pretty(PLOC$LON)

PLON = c(min(PLOC$LON), PLON[PLON>min(PLOC$LON)&PLON<max(PLOC$LON)], max(PLOC$LON))
     

plot(gxy$x, gxy$y,  asp=TRUE)

 addTIX(PLAT, PLON, PMAT=NULL, col='red', TICS=c(.1,.1), PROJ=PROJ)




   

}

\keyword{aplot}
