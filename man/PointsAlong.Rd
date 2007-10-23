\name{PointsAlong}
\alias{PointsAlong}

\title{Find spaced Points along a line}
\description{
  find evenly spaced points along a line
}
\usage{
PointsAlong(x, y, spacing = NULL, N = 1, endtol = 0.1)
}

\arguments{
  \item{x}{x-coordinates}
  \item{y}{y-coordinates}
  \item{spacing}{spacing of points}
  \item{N}{number of points}
  \item{endtol}{indent on either ends}
}
\details{
  The total length is returned: this is the line integral along the
  trace.
}
\value{
  List:
  \item{x}{x-coordinates}
  \item{y}{y-coordinates}
  \item{rot}{angle at the points}
  \item{TOT}{total length along the trace}
  
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu}

\examples{

plot(c(-5,5), c(-5,5), asp=1, type='n' )
ff=list()
ff$x=c(-4.850,-4.700,-3.934,-2.528, 0.603, 2.647, 3.861, 2.626)
ff$y=c(-4.045,-2.087,-0.710, 0.172, 1.291, 2.087,-0.753,-4.131)

g = PointsAlong(ff$x, ff$y, N=20)

lines(ff$x, ff$y)
points(g$x, g$y)

}

\keyword{misc}
