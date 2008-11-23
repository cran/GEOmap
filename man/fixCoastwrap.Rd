\name{fixCoastwrap}
\alias{fixCoastwrap}
\title{Correct the Wrapping problem}
\description{
  Correct wrapping for GEOmaps
}
\usage{
fixCoastwrap(Z, maxdis = 100)
}
\arguments{
  \item{Z}{list of x, y}
  \item{maxdis}{maximum distance for differences}
}
\details{
  Based on mapswrap program
}
\value{
  List:
  \item{x}{x-coordinates (longitudes)}
  \item{y}{y-coordinates (latitudes)}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\examples{
data(coastmap)
SEL = which(coastmap$STROKES$nam=="AFROASIA")

A = ExcludeGEOmap(coastmap, SEL, INOUT="in")

plot(A$POINTS$lon, A$POINTS$lat, type='n')

points(A$POINTS$lon, A$POINTS$lat, pch='.')

######  note that the map wraps around.

B = fixCoastwrap(list(x=A$POINTS$lon, y=A$POINTS$lat), 100)
 which(is.na(B$x))



lines(B)


polygon(B, col=rgb(.8,1, .8))



}
\keyword{misc}
