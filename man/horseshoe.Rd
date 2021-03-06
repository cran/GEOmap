\name{horseshoe}
\alias{horseshoe}

\title{Horseshoe Symbol}
\description{
 Draw a Horseshoe Symbol
}
\usage{
horseshoe(x, y, r1 = 1, r2 = 1.2, h1 = 0.5, h2 = 0.5, rot = list(cs = 1,
sn = 0), col = "black", lwd = lwd, fill=FALSE)
}

\arguments{
 
  \item{x}{x-coordinates}
  \item{y}{y-coordinates}
  \item{r1}{x-radius of curled part}
  \item{r2}{y-radius of curled part}
  \item{h1}{length of first leg}
  \item{h2}{length of 2nd leg}
  \item{rot}{rotation, cos, sine}
  \item{col}{color of teeth and line}
  \item{lwd}{line width}
  \item{fill}{logical, TRUE=fill}
  
}

\value{
 Grapical Side Effect
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu}

\seealso{PointsAlong}
\examples{

ff=list()
ff$x=c(-4.850,-4.700,-3.934,-2.528, 0.603, 2.647, 3.861, 2.626)
ff$y=c(-4.045,-2.087,-0.710, 0.172, 1.291, 2.087,-0.753,-4.131)

G =getsplineG(ff$x, ff$y, kdiv=20)
g = PointsAlong(G$x, G$y, N=5)

plot(c(-5,5), c(-5,5), asp=1, type='n' )
lines(G)

horseshoe(g$x  , g$y , r1=.5, r2=.8, h2=0, h1=0, rot=g$rot , col='blue')


###  to make a "warm front" use something liek this:
###  shorten r2 relative to r1, to get a more squat shape for the half-suns

plot(c(-5,5), c(-5,5), asp=1, type='n' )

w1=list()
w1$x=c(-1.208, 0.113, 1.242, 2.200, 2.349)
w1$y=c( 3.206, 2.280, 0.344,-2.560,-3.485)
G =getsplineG(w1$x, w1$y, kdiv=20)
lines(G)
g = PointsAlong(G$x, G$y, N=5)

horseshoe(g$x  , g$y , r1=.5, r2=.4, h2=0, h1=0, rot=g$rot , col='blue')




}

\keyword{aplot}
