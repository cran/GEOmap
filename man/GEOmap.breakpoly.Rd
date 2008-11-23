\name{GEOmap.breakpoly}
\alias{GEOmap.breakpoly}
\title{Break up a polygon }
\description{Break up a polygon
}
\usage{
GEOmap.breakpoly(Z, ww)
}
\arguments{
  \item{Z}{list, x,y locations }
  \item{ww}{vector of indecies where NAs occur }
}
\details{
  The NA values in Z represent breaks.  GEOmap.breakpoly
  breaks the polygon up into individual strokes.
  The beginning and the ending of the stroke are combined.
}
\value{
  \item{newx}{list of x values}
  \item{newy}{list of y values}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{fixCoastwrap, GEOmap.breakline}
\examples{

x=1:100
y = 1:100

ww = c(25, 53, 75)


A = list(x=x, y=y)

W = GEOmap.breakpoly(A , ww)




}
\keyword{misc}
