\name{CCcheck}
\alias{CCcheck}
\title{Counter Clockwise check}
\description{Check for counter-clockwise orientation
  for polygons.  Positive is counterclockwise.
}
\usage{
CCcheck(Z)
}
\arguments{
  \item{Z}{list(x,y) }
}
\details{ Uses sign of the area of the polygon to determine
  polarity.
}
\value{
  \item{j}{sign of area} 

}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
  Based on the idea calculated area of a polygon.
}

\examples{


Y=list()
Y$x=c(170,175,184,191,194,190,177,166,162,164)
Y$y=c(-54,-60,-60,-50,-26,8,34,37,10,-15)

plot(c(160, 200),c(-85, 85), type='n')
points(Y)
lines(Y)

CCcheck(Y)


Z = list(x=rev(Y$x), y=rev(Y$y))

CCcheck(Z)

}
\keyword{misc}
