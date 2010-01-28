\name{GEOmap-package}
\alias{GEOmap-package}
\alias{GEOmap}
\docType{package}
\title{
GEOmap
}
\description{
Topographic and Geologic Mapping
}
\details{
\tabular{ll}{
Package: \tab GEOmap\cr
Type: \tab Package\cr
Version: \tab 1.5-9\cr
Date: \tab 2010-01-28\cr
License: \tab GPL\cr
}
Set of routines for making Map Projections (forward and inverse), Topographic Maps, Perspective plots, geologi
cal databases, interactive plotting and selection of focus regions.
}
\author{
Jonathan M. Lees<jonathan.lees.edu>
Maintainer:Jonathan M. Lees<jonathan.lees@unc.edu>
}
\references{
Snyder, John P., Map Projections- a working manual, USGS, Professional Paper, 1987.

Lees, J. M., Geotouch: Software for Three and Four Dimensional GIS in the Earth Sciences, Computers & Geosciences, 26, 7, 751-761, 2000.

}
\note{

  \describe{

\item{High level plotting:}{
BASICTOPOMAP
DOTOPOMAPI
geoLEGEND
GEOsymbols
locworld
plotGEOmap
plotGEOmapXY
linesGEOmapXY
rectGEOmapXY
textGEOmapXY
pointsGEOmapXY
insideGEOmapXY
plotusa
plotUTM
plotworldmap
XSECDEM}

\item{PLOTTING:}{
circle
addLLXY addTIX
antipolygon
zebra
demcmap
setXMCOL
shade.col}

\item{Geological Map Symbols:}{
bcars
faultdip
faultperp
horseshoe
normalfault 
OverTurned
perpen
teeth
thrust
SynAnticline
SSfault}

\item{Data manipulation:}{
getGEOmap
boundGEOmap
SELGEOmap
geoarea
GEOTOPO
getGEOperim
GETXprofile
Lintersect
LOCPOLIMAP
pline
selectPOLImap
setplotmat
SETPOLIMAP
settopocol
subsetTOPO}

\item{Misc:}{
getgreatarc
ccw
difflon
DUMPLOC
fmod
getmem
getsplineG
ilocator
inpoly
inside
itoxyz
jpostscript
local.file
meshgrid
PointsAlong
polyintern
RESCALE}

\item{Projections:}{
setPROJ
projtype
GLOB.XY
XY.GLOB
MAPconstants
GCLCFR
lambert.cc.ll
lambert.cc.xy
lambert.ea.ll
lambert.ea.xy
lcgc
merc.sphr.ll
merc.sphr.xy
utmbox
utm.elps.ll
utm.elps.xy
utm.sphr.ll
utm.sphr.xy
stereo.sphr.ll
stereo.sphr.xy
equid.cyl.ll
equid.cyl.xy}
}


  
}



\keyword{ package }
\seealso{RSEIS
}
\examples{

################  projections
proj = setPROJ(type = 2, LAT0 =23, LON0 = 35)

### get lat-lon
LL = XY.GLOB(200, 300, proj)


##  find x-y again, should be the same
XY = GLOB.XY(LL$lat, LL$lon, proj)
XY
################

data(worldmap)
  KAMlat = c(48.5,  65)
    KAMlon = c(150, 171)
    
    PLOC=list(LON=KAMlon,LAT=KAMlat)


    PLON = seq(from=KAMlon[1], to=KAMlon[2], by=2)
    PLAT = seq(from=KAMlat[1], to=KAMlat[2], by=2)
    
    proj = setPROJ(2, LON0=mean(KAMlon), LAT0=mean(KAMlat))

 xy = GLOB.XY(KAMlat,  KAMlon , proj)
 kbox=list(x=range(xy$x, na.rm=TRUE), y=range(xy$y, na.rm=TRUE))

 plot(kbox$x,kbox$y, type='n', axes=FALSE, xlab="", ylab="", asp=1)
   plotGEOmapXY(worldmap, LIM=c(KAMlon[1], KAMlat[1], KAMlon[2],
KAMlat[2]),  add=TRUE, PROJ=proj, axes=FALSE, xlab="", ylab="" )

sqrTICXY(kbox , proj, side=c(1,2,3,4), LLgrid=TRUE, col=grey(.7) )
title("Crude Map of Kamchatka")
   
    
}
