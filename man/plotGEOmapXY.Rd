\name{plotGEOmapXY}
\alias{plotGEOmapXY}
\alias{plotGEOmap}

\title{Plot a GEO map}
\description{
 High Level plot of GEO map
}
\usage{
plotGEOmapXY(MAP, LIM = c(-180, -90, 180, 90), PROJ = list(), PMAT=NULL,
add = TRUE,  SEL=NULL , GRID = NULL, GRIDcol = 1,
MAPcol = NULL, cenlon = 0, shiftlon = 0, linelty = 1, linelwd = 1, NUMB = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MAP}{Map Structure}
  \item{LIM}{Lat-Lon limits}
  \item{PROJ}{Projection list }
  \item{PMAT}{Perspective matrix conversion}
  \item{add}{logical, TRUE= add to existing plot}
  \item{SEL}{Index vector of strokes to be used in plotting,default=NULL(use all that pass other tests) }
  \item{GRID}{logical, TRUE=add grid lines}
  \item{GRIDcol}{color for grid lines}
  \item{MAPcol}{color for maps}
  \item{cenlon}{center longitude of plot}
  \item{shiftlon}{degrees, rotate longitude}
  \item{linelty}{Line type}
  \item{linelwd}{line width}
  \item{NUMB}{logical, number the strokes on the map}
  \item{\dots}{graphical parameters}
}
\details{
  plotGEOmapXY includes projection of the data, though plotGEOmap does not.
}
\value{
  Graphical Side Effects
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{DOTOPOMAPI, addLLXY}
\examples{


PLOC=list(LON=c(137.008, 141.000), LAT=c(34.000, 36.992), x=c(137.008, 141.000), y=c(34.000, 36.992) )
  PROJ = setPROJ(type=2, LAT0=mean(PLOC$y) , LON0=mean(PLOC$x) )
      
gxy = GLOB.XY(PLOC$LAT, PLOC$LON, PROJ)
 PLAT =  pretty(PLOC$LAT)
    PLAT = c(min(PLOC$LAT),  PLAT[PLAT>min(PLOC$LAT) & PLAT<max(PLOC$LAT)],max(PLOC$LAT)) 
  PLON  = pretty(PLOC$LON)
        PLON = c(min(PLOC$LON), PLON[PLON>min(PLOC$LON) & PLON<max(PLOC$LON)], max(PLOC$LON))
     

plot(gxy$x, gxy$y,  asp=TRUE)
addLLXY(PLAT,  PLON, PROJ=PROJ, LABS=TRUE, PMAT=NULL, TICS=c(.1,.1) )



}

\keyword{hplot}
