\name{faultperp}
\alias{faultperp}

\title{Fault Perpendiculars}
\description{
 Draw perpendicular marks on fault trace
}
\usage{
faultperp(x, y, N = 20, endtol = 0.1, h = 1, col = "black")
}

\arguments{
    \item{x}{x-coordinates}
  \item{y}{y-coordinates}
   \item{N}{number of points}
 \item{endtol}{indent on either ends}
 
  \item{h}{length of perpendicular marks}
  \item{col}{color of line}
}

\value{
   Graphical Side effect
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{OverTurned}
\examples{

ff=list()
ff$x=c(-4.850,-4.700,-3.934,-2.528, 0.603, 2.647, 3.861, 2.626)
ff$y=c(-4.045,-2.087,-0.710, 0.172, 1.291, 2.087,-0.753,-4.131)

G =getsplineG(ff$x, ff$y, kdiv=20)
g = PointsAlong(G$x, G$y, N=5)


plot(c(-5,5), c(-5,5), asp=1, type='n' )
lines(G)

faultperp(G$x, G$y, N = 10, endtol = 0.1, h = .3, col = "black")


}

\keyword{aplot}
