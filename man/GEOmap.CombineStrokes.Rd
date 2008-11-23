\name{GEOmap.CombineStrokes}
\alias{GEOmap.CombineStrokes}
\title{Combine strokes in a GEOmap list }
\description{
  Combine strokes in a GEOmap list
}
\usage{
GEOmap.CombineStrokes(MAP, SEL)
}
\arguments{
  \item{MAP}{GEOmap list}
  \item{SEL}{index of strokes to be combined}
}
\details{
  Stokes are combined in the order
  designated by the SEL index vector.  The direction of the
  strokes is not modified - this may have to be
  fixed so that strokes align properly.
  
}
\value{
  GEOmap list
  \item{STROKES}{Metadata for strokes}
  \item{POINTS}{list, lat=vector, lon=vector} 
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{
GEOmap.cat, GEOmap.Extract, GEOmap.CombineStrokes, list.GEOmap
}
\examples{

data(coastmap)
SEL = which(coastmap$STROKES$nam=="Caribbean")


CAR =  GEOmap.Extract(coastmap, SEL, INOUT="in" )

plotGEOmap(CAR, MAPstyle=3, NUMB=TRUE)

CAR2 = GEOmap.CombineStrokes(CAR, SEL =c(6:15)  )

plotGEOmap(CAR2, MAPstyle=3, MAPcol='red' , add=TRUE)

}
\keyword{misc}
