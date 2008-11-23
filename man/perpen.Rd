\name{perpen}
\alias{perpen}

\title{perpendicular marks  along line}
\description{
draw perpendicular marks  along line
}
\usage{
perpen(x, y, h, rot, col = "black", lwd = 1)
}

\arguments{
  \item{x}{x-coordinates}
  \item{y}{y coordinates}
  \item{h}{height of tooth}
  \item{rot}{rotation of teeth}
  \item{col}{color of line}
  \item{lwd}{line width }
}
\details{
  Used by faultperp
}
\value{
 graphical side effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu}

\seealso{PointsAlong, faultperp}
\examples{
plot(c(-5,5), c(-5,5), asp=1, type='n' )
ff=list()
ff$x=c(-4.850,-4.700,-3.934,-2.528, 0.603, 2.647, 3.861, 2.626)
ff$y=c(-4.045,-2.087,-0.710, 0.172, 1.291, 2.087,-0.753,-4.131)

G =getsplineG(ff$x, ff$y, kdiv=20)
g = PointsAlong(G$x, G$y, N=5)

lines(G)

perpen(g$x, g$y, .3, g$rot, col = "black", lwd = 1)

}

\keyword{aplot}
