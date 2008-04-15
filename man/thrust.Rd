\name{thrust}
\alias{thrust}

\title{Thrust Fault}
\description{
  Add Thrust fault with teeth on overlying block
}
\usage{
thrust(x, y, h = 1, N=1, REV = FALSE, endtol=0.1, col = "black", ...)
}

\arguments{
  \item{x}{x-coordinates}
  \item{y}{y-coordinates}
  \item{h}{height of teeth}
  \item{N}{NUmber of points along line}
  \item{endtol}{ percent tolerance on ends of line}
  \item{REV}{reverse direction of x-y (teeth on other side) }
  \item{col}{color of teeth and line}
  \item{\dots}{graphical parameters}
}

\value{
 Graphical Side effect
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu}

\seealso{teeth}
\examples{
 plot(c(-5,5), c(-5,5), asp=1, type='n' )
  
ff=list()
ff$x=c(-4.850,-4.700,-3.934,-2.528, 0.603, 2.647, 3.861, 2.626)
ff$y=c(-4.045,-2.087,-0.710, 0.172, 1.291, 2.087,-0.753,-4.131)


###

 plot(c(-5,5), c(-5,5), asp=1, type='n' )
  
thrust(ff$x, ff$y,  h=2, N=14, REV=FALSE)


##########  reverse side:
plot(c(-5,5), c(-5,5), asp=1, type='n' )
  
thrust(ff$x, ff$y, h=2, N=14, REV=TRUE)





}

\keyword{aplot}
