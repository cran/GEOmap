\name{lamaz.eqarea}
\alias{lamaz.eqarea}
\title{Lambert-Azimuthal Equal Area}
\description{
  Map Projection (Lambert-Azimuthal Equal Area) for global plots.
}
\usage{
lamaz.eqarea(phi1, lam0, phi, lam)
}
\arguments{
  \item{phi1}{Central Latitude, radians}
  \item{lam0}{Central Longitude}
  \item{phi}{vector of Latitude, points for plotting, radians}
  \item{lam}{vector of Longitude, points for plotting , radians}
}

\value{
  \item{x}{position on the plot}
  \item{y}{position on the plot}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
  This is a projection routine that does not need
  to be set in advance. 
}
\seealso{setPROJ}
\examples{
data(coastmap)
#########  coastmap is a GEOmap list
DEGRAD = pi/180

phicen  = -90*DEGRAD
lamcen  = 0*DEGRAD

i = 7
j1 = coastmap$STROKES$index[i]+1
j2 = j1+ coastmap$STROKES$num[i]-1
lat = coastmap$POINTS$lat[j1:j2]*DEGRAD
lon = coastmap$POINTS$lon[j1:j2]*DEGRAD

 xy = lamaz.eqarea(phicen, lamcen,lat, lon)

plot(xy, asp=1, type='n')


 polygon(xy, col=grey(.8))

title("Antarctica")




}
\keyword{misc}
