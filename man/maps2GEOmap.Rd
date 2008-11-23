\name{maps2GEOmap}
\Rdversion{1.1}
\alias{maps2GEOmap}
\title{Convert maps data to GEOmap format
}
\description{Convert maps data to GEOmap format
}
\usage{
maps2GEOmap(zz, wx = 1, mapnam = "temp")
}
\arguments{
  \item{zz}{
    Output list from maps package
}
\item{wx}{
  vector of breaks (in maps these are NA)
}
\item{mapnam}{
  Name pasted on each stroke
}
}
\details{
  The program takes the output of maps and converts to a GEOmap
  strucuture.
  This code should work with GMT style map files too.
}
\value{
  GEOmap list.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
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
