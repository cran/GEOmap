\name{geoLEGEND}
\alias{geoLEGEND}

\title{Geological legend from GEOmap Structure}
\description{
  Create and add Geological legend from GEOmap Structure
}
\usage{
geoLEGEND(names, shades, zx, zy, nx, ny, side=1, cex=0.5)
}

\arguments{
  \item{names}{namesof units}
  \item{shades}{colorsof units}
  \item{zx}{width of box, mm}
  \item{zy}{height of box, mm}
  \item{nx}{number of boxes in x-direction}
  \item{ny}{number of boxes in y-direction }
  \item{side}{Side of the plot for the legend (1,2,3,4) }
  \item{cex}{Character expansion for text in legend }

  
}
\details{
  Adds geological legend based on information
  provided.  Legend is placed in margin.
}
\value{
  Graphical Side Effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{

  If plot is resized, should re-run this as the
  units depend on the screen size information and the transformation
  of user coordinates.
 
}

\examples{

\dontrun{

library(RPMG)
library(RSEIS)
library(GEOmap)
library(geomapdata)

data(cosogeol)
data(cosomap)
     data(faults)
     data(hiways)
     data(owens)

proj = cosomap$PROJ
   
XMCOL = setXMCOL()

newcol = XMCOL[cosogeol$STROKES$col+1]
cosocolnums = cosogeol$STROKES$col
cosogeol$STROKES$col = newcol
ss = strsplit(cosogeol$STROKES$nam, split="_")     

geo = unlist(sapply(ss  , "[[", 1))


UGEO = unique(geo)


mgeo = match( geo, UGEO )

gcol = paste(sep=".", geo, cosogeol$STROKES$col)


ucol = unique(gcol)

N = length(ucol)


spucol = strsplit(ucol,split="\\.")     

       
names = unlist(sapply(spucol  , "[[", 1))

shades = unlist(sapply(spucol  , "[[", 2))

ORDN = order(names)
### example:


par(mai=c(0.5, 1.5, 0.5, 0.5) )

 plotGEOmapXY(cosomap, PROJ=proj,  add=FALSE, ann=FALSE, axes=FALSE)
  

     plotGEOmapXY(cosogeol, PROJ=proj,  add=TRUE, ann=FALSE, axes=FALSE)
  
geoLEGEND(names[ORDN], shades[ORDN], .28, .14, 4, 16, side=2)

####
par(mai=c(0.5, 0.5, 1.0, 0.5) )

 plotGEOmapXY(cosomap, PROJ=proj,  add=FALSE, ann=FALSE, axes=FALSE)
  

     plotGEOmapXY(cosogeol, PROJ=proj,  add=TRUE, ann=FALSE, axes=FALSE)
  
geoLEGEND(names[ORDN], shades[ORDN], .28, .14, 16, 6, side=3)


####
par(mai=c(0.5, 0.5, 0.5, 1) )

 plotGEOmapXY(cosomap, PROJ=proj,  add=FALSE, ann=FALSE, axes=FALSE)
  

     plotGEOmapXY(cosogeol, PROJ=proj,  add=TRUE, ann=FALSE, axes=FALSE)
  
geoLEGEND(names[ORDN], shades[ORDN], .28, .14, 3, 16, side=4)


####
par(mai=c(1.5, 0.5, 0.5, 0.5) )

 plotGEOmapXY(cosomap, PROJ=proj,  add=FALSE, ann=FALSE, axes=FALSE)
  

     plotGEOmapXY(cosogeol, PROJ=proj,  add=TRUE, ann=FALSE, axes=FALSE)
  
geoLEGEND(names[ORDN], shades[ORDN], .28, .14, 16, 3, side=1)


}

}


\keyword{misc}

