\name{DOTOPOMAPI}
\alias{DOTOPOMAPI}

\title{Do Interactive world Topography}
\description{
 Interactive selction and plotting of world topography
}
\usage{
DOTOPOMAPI(TOPO = TOPO, themap = NULL, shiftlon = 0, ALOC = NULL, DOCONT = TRUE, DOIMG = TRUE, PNTS = NULL, PCOL = "red", PCH = 1, PCEX = 1, PS = FALSE, MAPPATH = NULL, polybase = NULL, USAmap = NULL, usacity = NULL, worldcity = NULL)
}

\arguments{
  \item{TOPO}{Topographic structure}
  \item{themap}{Map structure}
  \item{shiftlon}{rotation of map in Longitude}
  \item{ALOC}{previously set target}
  \item{DOCONT}{logical, TRUE=add contour}
  \item{DOIMG}{logical, TRUE=add contour}
  \item{PNTS}{Plot set of points on map}
  \item{PCOL}{color palette}
  \item{PCH}{plotting character}
  \item{PCEX}{character expansion}
  \item{PS}{logical, TRUE=make postscript}
  \item{MAPPATH}{path to map data base files}
  \item{polybase}{polygon structure for continents}
  \item{USAmap}{USA detailed map structure}
  \item{usacity}{list of USA cities with population}
  \item{worldcity}{list of world cities}
}
\details{
  Should select a small section of the map
  or the program will take a long time to interpolate.
}
\value{
  Interactive Graphics side effect
}

\author{Jonathan M. Lees<jonathan.lees.edu>}
\note{
  The program uses akima to interpolate so if
  there are a very large number of points this will take
  a long time.
  Recommend using this for smaller geographic regions.
  This program is an RPMG GUI  driver for BASICTOPOMAP
  
}
\seealso{BASICTOPOMAP, RPMG}
\examples{
library(geomapdata)
data(worldmap)
data(ETOPO5)


\dontrun{
###  this is in interactive mode with plotworldmap:
DOTOPOMAPI(TOPO=ETOPO5, themap=worldmap, shiftlon=180)

}

##  select a small region,  like the boot heel of italy

###########  italy:
Alocat = list(
lon=c(11.55842, 18.92308),
lat=c(35.71026, 47.05730),
LON=c(11.55842, 18.92308),
LAT=c(35.71026, 47.05730),
utmbox=list(x=32, y="S", lon=6,lat=32), 
x=c(191.5584, 198.9231),
y=c(35.71026, 47.05730),
UTM0=list(lam=9,phi=36),
shiftlon=180)


DOTOPOMAPI(TOPO=ETOPO5, themap=worldmap, ALOC=Alocat, shiftlon=180)



}

\keyword{iplot}
