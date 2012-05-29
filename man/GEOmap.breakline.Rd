\name{GEOmap.breakline}
\alias{GEOmap.breakline}
\title{Break a line at specified indeces into a list}
\description{
  Break a line at specified indices into a list
}
\usage{
GEOmap.breakline(Z, ww)
}
\arguments{
  \item{Z}{list of x,y location values}
  \item{ww}{index vector of break locations}
}

\value{
  \item{newx}{list x of strokes}
  \item{newy}{list y of strokes}
  
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}


\examples{

Y=list()
Y$x=c(170,175,184,191,194,190,177,166,162,164)
Y$y=c(-54,-60,-60,-50,-26,8,34,37,10,-15)

GEOmap.breakline(Y, 5)


}
\keyword{misc}
