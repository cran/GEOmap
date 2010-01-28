\name{KINOUT}
\alias{KINOUT}
\title{Map inside-outside
}
\description{Determine if strokes
  are in a target region
}
\usage{
KINOUT(MAP, LLlim, projtype = 2)
}
\arguments{
  \item{MAP}{GEOmap list 
}
  \item{LLlim}{list: lat lon limits
}
  \item{projtype}{local projection type
}
}
\details{The limits are used to calculate an origin
  and each point is projected accordingly.
  The x-y values are evaluated
  for being in or out of the target.
  A local projection is used - UTM (2) is the prefered
  projection.  
}
\value{Vector or indeces of strokes that intersect the target.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{ The mercator projections
  do not work well with this routine.
}


\seealso{inpoly, 
}
\examples{

data(worldmap)
data(coastmap)
L = list(lon=c(163.59, 182.95), lat=c(-48.998, -32.446))


k = KINOUT(worldmap,L, 2)

###  which strokes are these?

 print( worldmap$STROKES$nam[k] )


k = KINOUT(coastmap,L, 2)

 print( coastmap$STROKES$nam[k] )


testmap =  GEOmap.Extract(coastmap,k, INOUT="in" )

 plotGEOmap(testmap)



}
\keyword{misc}
