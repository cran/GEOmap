\name{sqrTICXY}
\alias{sqrTICXY}
\title{Tick marks for Square plot}
\description{Lat-Lon Tick marks and grid for Square plot
}
\usage{
sqrTICXY(prsurf, proj, side = c(1, 2, 3, 4), LLgrid = TRUE, col = "black", colt = "black", font=2, cex=1, lty=2, lwd=1, pcex=1, TICS=NULL)
}
\arguments{
  \item{prsurf}{list with x, y}
  \item{proj}{projection}
  \item{side}{vector, which sides to plot, 1=bottom, 2=left, 3=top, 4=right}
  \item{LLgrid}{logical, whether to add grid}
  \item{col}{color for grid}
  \item{colt}{color for text}
  \item{font}{default=2, font for labels}
 \item{cex}{character expansion for tic labels}
 \item{lty}{Line type for lines, default=2}
 \item{lwd}{Line width for lines, default=1}
 \item{pcex}{character expansion for tics, pch=2}
 \item{TICS}{list(lat, lon) this will replace the default}

}

\value{
 Graphical side effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{addLLXY, plotGEOmapXY}
\examples{

 KAMlat = c(48.5,  65)
    KAMlon = c(150, 171)
    proj = setPROJ( 2, LAT0=mean(KAMlat) , LON0=mean(KAMlon) )
    PLOC=list(LON=KAMlon,LAT=KAMlat)


    PLON = seq(from=KAMlon[1], to=KAMlon[2], by=2)
    PLAT = seq(from=KAMlat[1], to=KAMlat[2], by=2)
    
    proj = setPROJ(2, LON0=mean(KAMlon), LAT0=mean(KAMlat))
data(worldmap)

 plotGEOmapXY(worldmap, LIM=c(KAMlon[1], KAMlat[1], KAMlon[2], KAMlat[2]),
PROJ =proj, axes=FALSE, xlab="", ylab="" )

kbox = GLOB.XY( KAMlat,KAMlon, proj)
 sqrTICXY(kbox , proj, side=c(1,2,3,4), LLgrid=TRUE, col=grey(.7) )

#############  more detailed map:
data(kammap)

 plotGEOmapXY(kammap, LIM=c(KAMlon[1], KAMlat[1], KAMlon[2], KAMlat[2]),
PROJ =proj, axes=FALSE, xlab="", ylab="" )

kbox = GLOB.XY( KAMlat,KAMlon, proj)
 sqrTICXY(kbox , proj, side=c(1,2,3,4), LLgrid=TRUE, col=grey(.7) )
   


}
\keyword{misc}
