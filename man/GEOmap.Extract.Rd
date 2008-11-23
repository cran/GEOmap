\name{GEOmap.Extract}
\alias{GEOmap.Extract}
\alias{fastExtract}

\title{Extract from GEOmap}
\description{Extract or Exclude parts of a GEOmap list.
}
\usage{
GEOmap.Extract(MAP, SEL, INOUT = "out")
fastExtract(MAP, SEL, INOUT = "out")

}
\arguments{
  \item{MAP}{GEOmap List }
  \item{SEL}{Selection of stroke indeces to include or exclude }
  \item{INOUT}{text, "in" means include, "out" means exclude }
}
\value{
  \item{GEOmap}{list}
}
\details{
  Can either extract from the GEOmap data list with in, or exclude with
  out.
  fastExtract is the same but may be faster since it does not process
  all the strokes in the base GEOmap.
  
}
\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{GEOmap.cat, GEOmap.Extract, GEOmap.CombineStrokes, list.GEOmap,
  getGEOmap, plotGEOmap, SELGEOmap, boundGEOmap,   }
\examples{
data(coastmap)
SEL=which(coastmap$STROKES$nam=="AMERICAS")
NSAMER =  GEOmap.Extract(coastmap,SEL, INOUT="in" )
plotGEOmap(NSAMER)

}
\keyword{misc}
