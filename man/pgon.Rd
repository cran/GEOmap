\name{pgon}
\alias{pgon}
\title{Plot regular polygon: pentagon, hexagon, octagon}
\description{
  Plot regular polygon: pentagon, hexagon, octagon
}
\usage{
pgon(x,  y, siz=siz, col="black", border=NULL, K=5, startalph = -45, ... )
}
\arguments{
  \item{x}{x-coordinate }
  \item{y}{y-coordinate }
  \item{siz}{radius or size }
  \item{col}{inside color }
  \item{border}{border color }
  \item{K}{number of sides per polygon}
  \item{startalph}{starting angle }
  \item{\dots}{graphical parameters }
}
\details{
  I figure is resized needs to be re-called.
}
\value{
  Graphical Side Effects

}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\examples{

N = 25
x = rnorm(N)
y = rnorm(N)

z = rnorm(N)

######## draw pentagons
plot(x,y, type='n', axes=FALSE, ann=FALSE)
pgon(x,y, siz=abs(z)/10, col="white", border='black', startalph =60, K=5, lwd=.5, xpd=TRUE)

######   color the points, use 4-sided blocks
rbow=rainbow(100)

ss = sample(1:100, N, replace = TRUE, prob = NULL)
plot(x,y, type='n', axes=FALSE, ann=FALSE)
pgon(x,y, siz=abs(z)/10, col=rbow[ss], border='black', startalph =60, K=4, lwd=.5, xpd=TRUE)




}
\keyword{aplot}
