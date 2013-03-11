\name{plotGEOmap}
\alias{plotGEOmap}

\title{Plot a GEO map}
\description{
 High Level plot of GEO map
}
\usage{
plotGEOmap(MAP, LIM = c(-180, -90, 180, 90) ,
shiftlon = 0, add = TRUE ,
NUMB = FALSE , SEL = NULL, MAPcol = NULL,
MAPstyle = NULL, border=NA,
PLOT = TRUE, PRINT=FALSE, BB = FALSE, ...)
}

\arguments{
  \item{MAP}{Map Structure}
  \item{LIM}{Lat-Lon limits}
  \item{add}{logical, TRUE= add to existing plot}
  \item{SEL}{Index vector of strokes to be used in plotting,
    default=NULL(use all that pass other tests) }
  \item{MAPcol}{override color for maps}
  \item{MAPstyle}{override plotting style for maps}
  \item{border}{color, add border to polygons, NA=no border}
  \item{shiftlon}{degrees, rotate longitude}
  \item{NUMB}{logical, number the strokes on the map}
  \item{PLOT}{logical, TRUE=plot map, else just set up plotting area}
  \item{PRINT}{logical, TRUE=show selected stroke indeces on the
    screen(default=FALSE) }
  \item{BB}{logical, TRUE=add bounding box to each stroke (default=FALSE) }
  
  \item{\dots}{graphical parameters}
}
\details{
   plotGEOmap does
  not plot a projected map.  MAPcol and MAPstyle can be used to override the
  colors and style in the map-list.  These are applied to all the strokes.
  
}
\value{
  Graphical Side Effects
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{plotGEOmapXY, DOTOPOMAPI, addLLXY}
\examples{
library(geomapdata)

data(coastmap)

plotGEOmap(coastmap , xaxs='i', yaxs='i')

####################   example:
coastmap$STROKES$col[coastmap$STROKES$code=="C" ] = rgb(1, .6, .6)
coastmap$STROKES$col[coastmap$STROKES$code=="c" ] = rgb(1, .9, .9)
coastmap$STROKES$col[coastmap$STROKES$code=="L" ] = rgb(.6, .6, 1)

plot(c(-30, 370), c(-85, 85), type='n', ann=FALSE,  xaxs='i', yaxs='i')

plotGEOmap(coastmap , border='black' , add=TRUE)
title(xlab="Longitude", ylab="Latitude" )
grid()

box()


\dontrun{

###  political map of the world
library(geomapdata)
plotGEOmap(coastmap , border='black' , add=FALSE, xaxs='i')

data(europe.bdy)
data(asia.bdy)

data(africa.bdy)
data(namer.bdy)

data(samer.bdy)
data(USAmap)


plotGEOmap(europe.bdy ,  add=TRUE)
plotGEOmap(asia.bdy ,  add=TRUE)
plotGEOmap(africa.bdy ,  add=TRUE)
plotGEOmap(namer.bdy ,  add=TRUE)

plotGEOmap(samer.bdy ,  add=TRUE)

plotGEOmap(USAmap ,  add=TRUE)



}


}

\keyword{hplot}
