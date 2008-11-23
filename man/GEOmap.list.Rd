\name{GEOmap.list}
\alias{GEOmap.list}
\title{GEOmap to list}
\description{Inverse of list.GEOmap.
}
\usage{
GEOmap.list(MAP, SEL = 1)
}
\arguments{
  \item{MAP}{GEOmap list }
  \item{SEL}{index, selecttion of specific strokes }
}
\details{
  Returns the GEOmap strokes and instead of a long
  vector for the points they are broken down into
  a list of strokes.
  
 
}
\value{
  \item{STROKES}{Metadata for strokes}
  \item{POINTS}{list, lat=vector, lon=vector}
   \item{LL}{list of lat-lon strokes}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{GEOmap.cat, GEOmap.Extract, GEOmap.CombineStrokes, list.GEOmap}
\examples{

data(coastmap)
SEL=which(coastmap$STROKES$nam=='CUBA')
G = GEOmap.list(coastmap, SEL=SEL )

###  Lat-Lon of Cuba
G$LL



}
\keyword{misc}
