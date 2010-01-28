\name{targetLL}
\alias{targetLL}
\title{Target Lat-Lon
}
\description{Get a target Lat-Lon from a set of Lat-Lon pairs
}
\usage{
targetLL(sta, rdist = 100)
}
\arguments{
  \item{sta}{station list (with slots lat lon)
}
  \item{rdist}{radius in km
}
}
\details{Uses the Median station as the center and returns the lat-lon
  extents of the target region.
}
\value{list(
  
  \item{A}{ matrix with lat-lon pairs (lons=(0,360)  }
  \item{B}{  matrix with lat-lon pairs  (lons=(-180, 180))  }
  \item{mlat}{ median latitude }
  \item{mlon}{ median longitude } 
  \item{Jlat}{ range of lats }
  \item{Jlon}{  range of lons }
  \item{proj}{ projection list}
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\examples{

sta=list( lat=rnorm(10, mean=60, sd=0.5),
lon = rnorm(10, mean=60, sd=0.5))

A = targetLL(sta, rdist = 100)
print(A)


sta=list( lat=rnorm(10, mean=-30, sd=0.5),
lon = rnorm(10, mean=-40, sd=0.5))

A = targetLL(sta, rdist = 100)
print(A)





}
\keyword{misc}
