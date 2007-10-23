\name{OverTurned}
\alias{OverTurned}

\title{Plot Overturned fault}
\description{
Plot Overturned fault
}
\usage{
OverTurned(x, y, syn = TRUE, spacing = NULL, N = 1, r1 = 1, r2 = 1.2, h1 = 0.5, h2 = 0.5, endtol = 0.1, REV = FALSE, col = "black", ...)
}

\arguments{
  \item{x}{x-coordinates}
  \item{y}{y-coordinates}
  \item{syn}{logical, TRUE=syncline, FALSE=anticline}
  \item{spacing}{spacing of points}
  \item{N}{number of points}
  \item{r1}{x-radius of curled part}
  \item{r2}{y-radius of curled part}
  \item{h1}{length of first leg}
  \item{h2}{length of 2nd leg}
  \item{endtol}{indent on either ends}
  \item{REV}{reverse direction of x-y}
  \item{col}{color of teeth and line}
  \item{\dots}{graphical parameters}
}
\details{
 
}
\value{
 Graphical Side effect
}
\references{}
\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{PointsAlong}
\examples{

plot(c(-5,5), c(-5,5), asp=1, type='n' )
ff=list()
ff$x=c(-4.850,-4.700,-3.934,-2.528, 0.603, 2.647, 3.861, 2.626)
ff$y=c(-4.045,-2.087,-0.710, 0.172, 1.291, 2.087,-0.753,-4.131)


OverTurned(ff$x,ff$y,  r1= .4, r2= .8, h1= .5, h2= .5, N=5, syn=FALSE,
endtol=.2)



}

\keyword{aplot}
