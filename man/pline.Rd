\name{pline}
\alias{pline}

\title{Point to line distance}
\description{
  get sortest distance from arbitrary point to a segment.  
}
\usage{
pline(x1, y1, x2, y2, ex, ey)
}

\arguments{
  \item{x1}{x coordinate segment start}
  \item{y1}{y coordinate segment start}
  \item{x2}{x coordinate segment end }
  \item{y2}{y coordinate segment end }
  \item{ex}{x, point }
  \item{ey}{y point}
}

\value{
vector of:  
  \item{dis}{distance to segment}
  \item{dee}{distance to line}
  \item{zee}{projection along line}
  \item{px}{x, point of intersection}
  \item{py}{y, point of intersection}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{polyintern}
\examples{
L=list()
L$x=c(-0.161416832868, 0.484046270443,-0.472622257679)
L$y=c(-0.735779816514, 0.306422018349, 0.192660550459)

P = pline(L$x[1], L$y[1], L$x[2], L$y[2], L$x[3], L$y[3])

plot(L$x, L$y, type='n', asp=1)
segments(L$x[1], L$y[1], L$x[2], L$y[2])
points( L$x[3], L$y[3])

segments(L$x[3], L$y[3], P[4], P[5], col='red')




}

\keyword{misc}
