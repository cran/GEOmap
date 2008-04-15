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
Version: \tab 1.0-9\cr
Date: \tab 2008-04-15\cr
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

\item{High level plotting:}{
BASICTOPOMAP
DOTOPOMAPI
geoLEGEND
GEOsymbols
locworld
plotGEOmap
plotGEOmapXY
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
boundGEOmap
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
ccw
difflon
DUMPLOC
fmod
getmem
getspline
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



\keyword{ package }
\seealso{RSEIS
}
\examples{
proj = setPROJ(type = 2, LAT0 =23, LON0 = 35)

### get lat-lon
LL = XY.GLOB(200, 300, proj)


##  find x-y again, should be the same
XY = GLOB.XY(LL$lat, LL$lon, proj)
XY


}
