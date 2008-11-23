\name{GEOmap.cat}
\alias{GEOmap.cat}
\title{Concatenate Two GEOmaps}
\description{Combine Two GEOmaps into one
}
\usage{
GEOmap.cat(MAP1, MAP2)
}
\arguments{
  \item{MAP1}{GEOmap list}
  \item{MAP2}{GEOmap list}
}
\details{
  Maps are combine consecutively.
}
\value{
  \item{GEOmap}{list}

}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{GEOmap.Extract, GEOmap.CombineStrokes, list.GEOmap}
\examples{

data(coastmap)
CUBA = GEOmap.Extract(coastmap,90, INOUT="in" )

NSAMER =  GEOmap.Extract(coastmap,2, INOUT="in" )
AMAP = GEOmap.cat(CUBA, NSAMER)
plotGEOmap(AMAP )

}
\keyword{misc}
