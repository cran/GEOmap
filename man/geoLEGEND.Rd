\name{geoLEGEND}
\alias{geoLEGEND}

\title{Geological legend from GEOmap Structure}
\description{
  Create and add Geological legend from GEOmap Structure
}
\usage{
geoLEGEND(names, shades, zx, zy, nx, ny)
}

\arguments{
  \item{names}{namesof units}
  \item{shades}{colorsof units}
  \item{zx}{width of box, mm}
  \item{zy}{height of box, mm}
  \item{nx}{number of boxes in x-direction}
  \item{ny}{ number of boxes in y-direction }
}
\details{
  Adds geological legend based on information
  provided.  Legend is placed in margin
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

data(cosogeol)
data(cosomap)
     data(faults)
     data(hiways)
     data(owens)

proj = cosomap$PROJ
     plotGEOmapXY(cosomap, PROJ=proj,  add=FALSE, ann=FALSE, axes=FALSE)
  

     plotGEOmapXY(cosogeol, PROJ=proj,  add=TRUE, ann=FALSE, axes=FALSE)
  
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

geoLEGEND(names[ORDN], shades[ORDN], .28, .14, 16, 6)


}

}


\keyword{misc}

