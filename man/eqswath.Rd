\name{eqswath}
\alias{eqswath}

\title{Extract a set of eathquakes in swath along a cross sectional line}
\description{
Extract a set of eathquakes in swath along a cross sectional line
}
\usage{
eqswath(x, y, z, L, width = 1, PROJ = NULL)
}

\arguments{
  \item{x}{x-coordinates of earthquakes}
  \item{y}{y-coordinates of earthquakes}
  \item{z}{z-coordinates of earthquakes}
  \item{L}{list of x-y coordinates of cross section}
  \item{width}{width of swath (km)}
  \item{PROJ}{projection information}
}
\details{
  All units should be the same.
}
\value{

 \item{r}{r-distance along cross section (x-coordinate)}
  \item{dh}{distance from cross seection}
  \item{depth}{depth in cross section (y-coordinate) }
  \item{flag}{index vector of which earthquakes fell in swath and depth range}
    \item{InvBox}{coordinates of swath for plotting on map}
  
 
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{XSECwin, XSECEQ}
\examples{

 #############  create data
x = runif(100, 1, 100)  
y = runif(100, 1, 100) 
z = runif(100, 1, 10) 
plot(x,y, asp=1)
## L = locator()

L=list()
L$x=c( 5.42328560757,64.62879777806)
L$y=c(89.843266449785,-0.174423911329)

J = eqswath(x, y, z, L, width = 10, PROJ = NULL)

##########   show box:
plot(x,y, asp=1)
lines(J$InvBox$x, J$InvBox$y)


############  show cross section with events plotted
plot(J$r, -J$depth)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{misc}
