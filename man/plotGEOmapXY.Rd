\name{plotGEOmapXY}
\alias{plotGEOmapXY}

\title{Plot a projected GEO map }
\description{
 High Level plot of GEO map
}
\usage{
plotGEOmapXY(MAP, LIM = c(-180, -90, 180, 90), PROJ = list(), PMAT=NULL,
add = TRUE,  SEL=NULL , GRID = NULL, GRIDcol = 1,
MAPcol = NULL, MAPstyle = NULL, border = NA, cenlon = 0, shiftlon = 0, linelty = 1,
linelwd = 1, ptpch=".", ptcex=1, NUMB = FALSE, ...)
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
  \item{MAPcol}{override color for maps}
  \item{MAPstyle}{override plotting style for maps}
  \item{border}{color, add border to polygons, NA=no border}
  \item{cenlon}{center longitude of plot}
  \item{shiftlon}{degrees, rotate longitude}
  \item{linelty}{Line type}
  \item{linelwd}{line width}
  \item{ptpch}{plotting character for strokes (style=1) that are plotted
  as points}
  \item{ptcex}{character expansion factor for style=1 strokes }

  
  \item{NUMB}{logical, number the strokes on the map}
  
  
  \item{\dots}{graphical parameters}
}
\details{
  plotGEOmapXY includes projection of the data, plotGEOmap does
  not.  MAPcol and MAPstyle can be used to override the
  colors and style in the map-list.  These are applied to all the
  strokes.

  For strokes that are of style=1 points are plotted with graphical
  parameters ptpch="." and ptcex=1 unless otherwise indicated.
  
  
  
}
\value{
  Graphical Side Effects
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{DOTOPOMAPI, addLLXY, plotGEOmap}
\examples{

data(japmap)

PLOC=list(LON=c(137.008, 141.000), LAT=c(34.000, 36.992), x=c(137.008, 141.000), y=c(34.000, 36.992) )
  PROJ = setPROJ(type=2, LAT0=mean(PLOC$y) , LON0=mean(PLOC$x) )
      
gxy = GLOB.XY(PLOC$LAT, PLOC$LON, PROJ)
 PLAT =  pretty(PLOC$LAT)
    PLAT = c(min(PLOC$LAT),  PLAT[PLAT>min(PLOC$LAT) & PLAT<max(PLOC$LAT)],max(PLOC$LAT)) 
  PLON  = pretty(PLOC$LON)
        PLON = c(min(PLOC$LON), PLON[PLON>min(PLOC$LON) & PLON<max(PLOC$LON)], max(PLOC$LON))
     

plot(gxy$x, gxy$y,  asp=TRUE, ann=FALSE , axes=FALSE)

plotGEOmapXY(japmap, LIM=c(PLOC$LON[1], PLOC$LAT[1],PLOC$LON[2],
     PLOC$LAT[2]) , PROJ=PROJ, add=TRUE )

addLLXY(PLAT,  PLON, PROJ=PROJ, LABS=TRUE, PMAT=NULL, TICS=c(.1,.1) )

###############



####  rotated map
PMAT = rotdelta4(-34)

 plotGEOmapXY(japmap, PMAT=PMAT, xpd=TRUE)



}

\keyword{hplot}
