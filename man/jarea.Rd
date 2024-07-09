\name{jarea}
\alias{jarea}
\title{
Area of closed polygon.
}
\description{Returns area of polygon.
}
\usage{
jarea(L)
}
\arguments{
  \item{L}{ list with x,y components
}
}
\details{If polygon is counter clockwise (CCW) area will be positive,
  else negative.  If not sure, take absolute value of output.
}
\value{Area in dimensions of x,y
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{
set.seed(12)
X = runif(10, 1, 100)
    Y = runif(10, 1, 100)

    hc = chull(X, Y)
#### looks like chull returns points in clockwise
    L = list(x=X[hc] , y=Y[hc] )

  j1 = jarea(L )

#########  reverse order of polygon
jc = rev(hc)
    L = list(x=X[jc] , y=Y[jc] )
 j2 = jarea(L )



}
\keyword{misc}
