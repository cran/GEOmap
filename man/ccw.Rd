\name{ccw}
\alias{ccw}
\title{Counter Clockwise Whorl}
\description{
  Used for determining if points are in polygons.
}
\usage{
ccw(p0, p1, p2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p0}{point 0 }
  \item{p1}{point 1  }
  \item{p2}{point 2   }
}

\value{
 returns 1 or 0 depending on position of points
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{Lintersect}
\examples{

l1 = list(p1=list(x=0, y=0), p2=list(x=1,y=1))
l2 = list(p1=list(x=6, y=4), p2=list(x=-1,y=-12))

ccw(l1$p1, l1$p2, l2$p1)


}

\keyword{misc}
