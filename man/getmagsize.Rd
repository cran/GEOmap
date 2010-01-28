\name{getmagsize}
\alias{getmagsize}
\title{Earthquake Magnitude based on exponentional
}
\description{Estimate a size for plotting earthqukes
  recorded as a logarithmic scale
}
\usage{
getmagsize(mag, minsize = 1, slope = 1, minmag = 0, maxmag = 8, style = 1)
}
\arguments{
  \item{mag}{magnitudes from catalog
}
  \item{minsize}{minimum size
}
  \item{slope}{slope for linear scale
}
  \item{minmag}{min magnitude
}
  \item{maxmag}{max magnitude
}
  \item{style}{Style of plotting: 0= all the same size; 1(default):
    exponential scale; 2=linear scale
}
}
\details{
  The idea is to have a scale reflect the
  size of the earthquake.
  The default style (1) has a few parameters left over
  from old program geotouch.

  
}
\value{
  vector of sizes for plotting
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}



\examples{

mag = 0:9

x = runif(10, 1, 100)
y  = runif(10, 1, 100)

g = getmagsize(mag)

plot(c(0, 100), c(0, 100), asp=1, type='n')

points(x, y, pch=1, cex=g)




}
\keyword{misc}
