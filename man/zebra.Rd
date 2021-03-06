\name{zebra}
\alias{zebra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Horizontal Zebra Scale}
\description{
  Plot a zebra style horizontal scale on a projected map.
}
\usage{
zebra(x, y, Dx, dx, dy, lab = "", pos=1, col = c("black", "white"),
cex = 1, textcol="black", xpd=TRUE, PMAT = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{x-coordinate of left corner}
  \item{y}{y-coordinate of left corner}
  \item{Dx}{distance in x, km}
  \item{dx}{distance for zebra stripes in x }
  \item{dy}{thickness in km}
  \item{lab}{labels}
  \item{pos}{position of text, 1=below, 3=above, as in par}
  \item{col}{2-vector of colors, for the alternating bars}
  \item{cex}{character expansion}
  \item{textcol}{color for the text}
  
  \item{xpd}{logical, graphic parameter for clipping (see par) }
  \item{PMAT}{3D projection matrix from persp}
}
\details{
  Plots a zebra style kilometer scale on the current plot
}
\value{
 Graphical Side effect
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}


\examples{

library(geomapdata)

data(USAmap)
USALL=list()
USALL$lat=c(24.72853,49.62741)
USALL$lon=c(229.29389,296.41803)
## set UTM projection
PROJ = setPROJ(type = 2, LAT0 =mean(USALL$lat), LON0 = mean(USALL$lon) )

####  plot with UTM  projection:
plotGEOmapXY(USAmap, LIM= c(USALL$lon[1], USALL$lat[1],
     USALL$lon[2], USALL$lat[2]    )  , PROJ=PROJ, add=FALSE, shiftlon=0)

zeb=list()
zeb$x=c(197.727896066)
zeb$y=c(-1155.81158234)

zebra(zeb$x[1],zeb$y[1], 1000, 100, 60, lab="Km", cex=.6)




}

\keyword{aplot}
