\name{rotateGEOmap}
\Rdversion{1.1}
\alias{rotateGEOmap}
\title{Rotate a GEOmap
}
\description{
  Rotate a GEOmap to a new location on the globe
}
\usage{
rotateGEOmap(INmap, TARGlat, TARGlon, LAT0, LON0, beta = 0)
}
\arguments{
  \item{INmap}{Input GEOmap}
\item{TARGlat}{ Target center latitide}
\item{TARGlon}{ Target center longitide}
\item{LAT0}{  Source center latitide}
\item{LON0}{ Source center longitide}
\item{beta}{ rotation through axis coming out of screen}
}
\details{
  This function is used to translate a given map region
  to another for over plotting.  You can compare the
  areas of two region using the same projection.
  
}
\value{
  GEOmap list.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{plotGEOmapXY
}
\examples{
library(maps)


zz = map('state', region = c('new york', 'new jersey', 'penn'))

neweng  = maps2GEOmap(zz)

plotGEOmap(neweng)
##  L1 = locator(1)
L1=list()
L1$x=c(283.671347071854)
L1$y=c(42.008587074537)

LIMS1 = list( lon=range(neweng$POINTS$lon), lat=range(neweng$POINTS$lat) )

LIMS = c(LIMS1$lon[1], LIMS1$lat[1], LIMS1$lon[2], LIMS1$lat[2])

##########   prepare maps 2:

z2 = map('world', region = c('iceland'))
ice   = maps2GEOmap(z2)
plotGEOmap(ice)

##  L2 = locator(1)
L2=list()
L2$x=c(341.146812632372)
L2$y=c(64.9180246121089)

############    this version here is nicer, but required WORLMAP2
###kice = grep('ice' , coast2$STROKES$nam, ignore.case =TRUE)

### ice = GEOmap.Extract(coast2, kice  ,"in")

MAP = rotateGEOmap(ice, L1$y ,  L1$x ,  L2$y ,  L2$x, beta=-90 )


proj = setPROJ( 2, LAT0=L1$y, LON0=L1$x )

plotGEOmapXY(neweng, LIM=LIMS,  PROJ =proj, axes=FALSE, xlab="", ylab="" )


plotGEOmapXY(MAP, LIM=LIMS,  PROJ =proj, axes=FALSE, xlab="", ylab="", add = TRUE, MAPcol = grey(.85)  , lwd=2, xpd=TRUE)



  plotGEOmapXY(neweng, LIM=LIMS,  PROJ =proj, axes=FALSE, xlab="", ylab="", add=TRUE )
  



}
\keyword{misc}
