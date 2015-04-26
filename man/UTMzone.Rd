\name{UTMzone}
\alias{UTMzone}
\title{UTM zone information
}
\description{Return the UTM zone information
}
\usage{
UTMzone(lat, lon = NA)
}
\arguments{
  \item{lat}{latitude
}
  \item{lon}{longitude
}
}
\details{
  The function works two ways:
  If the lat-lon are numeric and lon is not NA
  then the UTM zone information is returned.
  If lon is NA and lat is one of the
  UTM zones, then the lat-lon information for that
  zone is returned.
}
\value{
  list:
  \item{zone}{Character, zone designation}
  \item{LON}{longitude range of the zone}
  \item{LAT}{latitude  range of the zone}
  \item{CEN}{center of the zone, used for projections}
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{setPROJ, UTM.xy, UTM.ll, DATUMinfo
}
\examples{
lat = 40.5
  lon = -73.50
UTMzone(lat, lon)
##  or
UTMzone("18T")
}
\keyword{misc}
