\name{antipolygon}
\alias{antipolygon}
\title{Fill the complement of a polygon}
\description{
 Fill a plot with a color outside the confines of a polygon.
}
\usage{
antipolygon(x, y, col = 0, corner=1, pct=.4)
}

\arguments{
  \item{x}{x coordinates of polygon}
  \item{y}{y coordinates of polygon}
  \item{col}{Fill color}
  \item{corner}{Corner on the plot to connect to at the end: 1 =
    LowerLeft(default) ; 2:UpperLeft 3 = UpperRight; 4=LowerRight}
  \item{pct}{Decimal percent of usr coordinates to expand beyond the polygon}

  
  
}
\details{
  antipolygon uses par("usr") to determine the external bounds of
  plotting region.
  Corners are labels from bottom left  counter-clockwise, 1-4.
  
  
  
}
\value{
List:
 \item{x}{ x-coordinates of mask }
 \item{y}{ y-coordinates of mask }			
 Used for graphical side effect	  
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}
\note{
  If the figure is resized after plotting, filling may not appear correct.
}
\seealso{polygon, par}
\examples{
set.seed(2018)
x = runif(100)
y = runif(100)

#########  some data points to plot:

plot(x,y)
###########   create polygon:
pp =list(x=c(0.231,0.316,0.169,0.343,0.311,0.484,0.757,
         0.555,0.800,0.563,0.427,0.412,0.203),
     y=c(0.774,0.622,0.401,0.386,0.138,0.312,0.200,0.459,
        0.658,0.624,0.954,0.686,0.813))

polygon(pp)

antipolygon(x=pp$x, y=pp$y,col='blue')
####  where as this does not look so good
plot(x,y)
antipolygon(x=pp$x, y=pp$y,col='blue', corner=2)



}

\keyword{aplot}
