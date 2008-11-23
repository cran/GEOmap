\name{list.GEOmap}
\alias{list.GEOmap}
\title{List stroke points in a GEOmap}
\description{List stroke points in a GEOmap
}
\usage{
list.GEOmap(MAP, SEL = 1)
}
\arguments{
  \item{MAP}{GEOmap list, with LL list }
  \item{SEL}{index, selecttion of specific strokes}
}
\details{
   Returns a GEOmap list from the output of list.GEOmap.
  This is used to repack a GEOmap list.
  
}
\value{
  GEOmap list
  \item{STROKES}{Metadata for strokes}
  \item{POINTS}{list, lat=vector, lon=vector}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{GEOmap.cat, GEOmap.Extract, GEOmap.CombineStrokes, list.GEOmap}
\examples{

data(coastmap)

length(coastmap$STROKES$nam)


G = GEOmap.list(coastmap, 1)

length(G$STROKES$nam)

H = list.GEOmap(G)

length(H$STROKES$nam)

plotGEOmap(H)

}
\keyword{misc}
