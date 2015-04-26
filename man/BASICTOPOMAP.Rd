\name{BASICTOPOMAP}
\alias{BASICTOPOMAP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Basic Topogrpahy Map}
\description{
  Basic Topogrpahy Map
}
\usage{
BASICTOPOMAP(xo, yo, DOIMG, DOCONT, UZ, AZ, IZ, perim, PLAT, PLON,
PROJ = PROJ, pnts = NULL, GRIDcol = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xo}{vector of x-coordinates}
  \item{yo}{vector of y-coordinates}
  \item{DOIMG}{logical, add image}
  \item{DOCONT}{logical, add contours}
  \item{UZ}{matrix of image values under  sea level}
  \item{AZ}{matrix of image values above sea level}
  \item{IZ}{matrix of image values}
  \item{perim}{perimeter vectors}
  \item{PLAT}{latitudes for tic-marks}
  \item{PLON}{longitude for tic-marks}
  \item{PROJ}{projection list}
  \item{pnts}{points to add to plot}
  \item{GRIDcol}{color for grid}
}
\details{
 Image is processed prior to calling 
}
\value{
  Graphical Side effects
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{DOTOPOMAPI, GEOTOPO}
\examples{

\dontrun{


library(geomapdata)
library(MBA) ##  for interpolation
#######  set up topo data
data(fujitopo)
#####  set up map data
data('japmap', package='geomapdata' )


###  target region
PLOC= list(LON=c(138.3152, 139.0214), 
LAT=c(35.09047, 35.57324))

PLOC$x =PLOC$LON
PLOC$y =PLOC$LAT



####  set up projection
PROJ = setPROJ(type=2, LAT0=mean(PLOC$y) , LON0=mean(PLOC$x) )

##########  select data from the topo data internal to the target
    topotemp = list(lon=fujitopo$lon, lat= fujitopo$lat, z=fujitopo$z)

    
 ####  project target
  A = GLOB.XY(PLOC$LAT  , PLOC$LON ,  PROJ)

#######   select topo
selectionflag = topotemp$lat>+PLOC$LAT[1] & topotemp$lat<=PLOC$LAT[2] &
topotemp$lon>+PLOC$LON[1] & topotemp$lon<=PLOC$LON[2]


###  project topo data
  B = GLOB.XY( topotemp$lat[selectionflag] ,topotemp$lon[selectionflag] ,  PROJ)

###  set up out put matrix:
### xo = seq(from=range(A$x)[1], to=range(A$x)[2], length=200)
###    yo = seq(from=range(A$y)[1], to=range(A$y)[2], length=200)

#######  interpolation using akima
  ###  IZ = interp(x=B$x , y=B$y,  z=topotemp$z[selectionflag]  , xo=xo, yo=yo)
DF = cbind(x=B$x , y=B$y ,  z=topotemp$z[selectionflag])
 IZ = mba.surf(DF, 200, 200, extend=TRUE)$xyz.est

    xo = IZ[[1]]
    yo = IZ[[2]]


###  image(IZ)

#######  underwater section
    UZ = IZ$z
    UZ[IZ$z>=0] = NA
#### above sea level
    AZ = IZ$z
    AZ[IZ$z<=-.01] = NA

#### create perimeter:
    perim= getGEOperim(PLOC$LON, PLOC$LAT, PROJ, 50)

###  lats for tic marks:
    PLAT =  pretty(PLOC$LAT)

    PLAT = c(min(PLOC$LAT),
PLAT[PLAT>min(PLOC$LAT) & PLAT<max(PLOC$LAT)],max(PLOC$LAT)) 
PLON  = pretty(PLOC$LON)

### main program:
 DOIMG = TRUE
DOCONT = TRUE
PNTS  = NULL

BASICTOPOMAP(xo, yo , DOIMG, DOCONT, UZ, AZ, IZ, perim, PLAT, PLON,
PROJ=PROJ, pnts=NULL, GRIDcol=NULL)


###  add in the map information
 plotGEOmapXY(japmap, LIM=c(PLOC$LON[1], PLOC$LAT[1],PLOC$LON[2],
PLOC$LAT[2]) , PROJ=PROJ, add=TRUE )

}


}

\keyword{misc}
