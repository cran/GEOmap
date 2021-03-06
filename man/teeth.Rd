\name{teeth}
\alias{teeth}

\title{Add Teeth to line}
\description{
 Add teeth marks to a line.
}
\usage{
teeth(x, y, h, rot, col = "black", border = "black")
}

\arguments{
  \item{x}{x-coordinates}
  \item{y}{y coordinates}
  \item{h}{height of tooth}
  \item{rot}{rotation of teeth}
  \item{col}{color of line}
  \item{border}{color of border, default= col}
}
\details{
  The rotation is usually determined by
  consecutive  x-y points
}
\value{
 Graphical Side effect
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu}

\seealso{thrust}
\examples{
 plot(c(-5,5), c(-5,5), asp=1, type='n' )
  
ff=list()
ff$x=c(-4.850,-4.700,-3.934,-2.528, 0.603, 2.647, 3.861, 2.626)
ff$y=c(-4.045,-2.087,-0.710, 0.172, 1.291, 2.087,-0.753,-4.131)

lines(ff)
points(ff)

### thrust uses teeth
thrust(ff$x, ff$y, h=2, N=12, REV=FALSE)



}

\keyword{aplot}
