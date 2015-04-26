\name{UTM.ll}
\alias{UTM.ll}
\alias{utm.wgs84.ll}

\title{Map projection}
\description{
  UTM  Map projection parameters supplied and X-Y, return the LAT-LON
  values, WGS-84
}
\usage{
UTM.ll(x , y , PROJ.DATA)
utm.wgs84.ll(x , y , PROJ.DATA)
}

\arguments{
  \item{x}{x}
  \item{y}{y}
  \item{PROJ.DATA}{list of projection parameters}
}

\value{
 List
  \item{phi}{Latitude-coordinate}
  \item{lam}{Longitude-coordinate}
}
\references{Snyder}
\author{Jonathan M. Lees<jonathan.lees.edu>}
\note{
  When calling the conversion from LL to XY or vice versa,
  convert the lon to 0 to 360.  Use RPMG::fmod for this conversion.
  This may be rectified in future revisions.
}
\seealso{setPROJ, GLOB.XY,  projtype,  utm.sphr.ll, UTMzone, plotUTM, utmbox, DATUMinfo}
\examples{
lat = 40.5
lon = -73.50
LON = RPMG::fmod(lon, 360)

uzone = UTMzone(lat, lon)
lon0 = uzone$CEN[2]
####  clark1866
wproj8 = setPROJ(type = 8, LAT0 = 0 , LON0 = lon0,  IDATUM=8)
uu = UTM.xy(lat, LON  , wproj8)
 UTM.ll(uu$x, uu$y ,wproj8)

###  wgs84
wproj1 = setPROJ(type = 8, LAT0 = 0 , LON0 = lon0  , IDATUM=1)
uu = UTM.xy(lat,LON  , wproj1)

 UTM.ll(uu$x, uu$y ,wproj1)




}
\keyword{misc}
