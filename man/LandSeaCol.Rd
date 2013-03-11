\name{LandSeaCol}
\Rdversion{1.1}
\alias{LandSeaCol}
\title{Land and Sea Colors
}
\description{Color pixels with two
  palettes, one for land the other for sea.
  
}
\usage{
LandSeaCol(IZ, coastmap, PROJ, calcol = NULL)
}
\arguments{
  \item{IZ}{list of x, y, z suitable for plotting
    with image or contour.
}
  \item{coastmap}{coastal map from GEOmap
}
  \item{PROJ}{projection list
}
  \item{calcol}{color map for the land
}
}
\details{The program uses closed polygons in
  the map list to separate the pixels
  into land versus sea.  Sea is colored with
  a palette of blues, land is colored according to
  topographic color scheme extracted from
  palettes similar to GMT palettes.

  All map and pixel coordinates are
  projected with the same projection parameters.
  calculations are done in XY coordinates.
  
}
\value{

  \item{Cmat}{Matrix of colors for each pixel}
  \item{UZ}{Under water }
  \item{AZ}{Above Sea Level}
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{settopocol, TOPOCOL
  
}
\examples{

\dontrun{

Lat.range = c(-10, 30)
Lon.range = c(65, 117)
######

########  load up the important libraries
 library(RFOC)

 library(geomapdata)

 data(coastmap)

     data(ETOPO5)
 PLOC=list(LON=Lon.range,LAT=Lat.range,lon=Lon.range,lat=Lat.range,
                 x=Lon.range, y=Lat.range )


#####   set up topography colors
COLS = settopocol()

####  set the projection ##   utm
 PROJ = setPROJ(type=2, LAT0=mean(PLOC$y) , LON0=mean(PLOC$x) )   
 NK = 300
    
   ###   extract topography from the etopo5 data base in geomapdata
     JMAT = GEOTOPO(ETOPO5, PLOC, PROJ, COLS$calcol,nx=NK, ny=NK )
#####   select relevant earthquakes

IZ = list(x=JMAT$xo, y=JMAT$yo, z=JMAT$IZ$z)

CMAT = LandSeaCol(IZ, coastmap, PROJ, calcol=NULL)

Mollist =CMAT$Cmat
dMol = attr(Mollist, "Dcol")

     ####  Under water
UZ = CMAT$UZ
    #####  above water
AZ = CMAT$AZ
    ####   blues for underwater:
   blues = shade.col(100, acol=as.vector(col2rgb("darkblue")/255),
        bcol= as.vector(col2rgb("paleturquoise")/255))

plot(x=range(IZ$x), y=range(IZ$y),
         type='n', asp=1, axes=FALSE, ann=FALSE)

image(x=IZ$x, y=IZ$y, z=(UZ), col=blues, add=TRUE)

image(x=IZ$x, y=IZ$y, z=(AZ), col=terrain.colors(100) , add=TRUE)

plotGEOmapXY(coastmap,
       LIM = c(Lon.range[1],Lat.range[1] ,Lon.range[2] ,Lat.range[2]),
       PROJ =PROJ,MAPstyle =2,MAPcol ="black" ,   add=TRUE  )




}


}
\keyword{misc}
