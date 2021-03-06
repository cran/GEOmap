\name{UTM.xy}
\alias{UTM.xy}
\alias{utm.wgs84.xy}
\title{Map projection}

\description{
 UTM  Map projection parameters supplied and LAT-LON, return the x-y
 values,  WGS-84 datum
}
\usage{
UTM.xy(phideg,  lamdeg, PROJ.DATA)
utm.wgs84.xy(phideg,  lamdeg, PROJ.DATA)
}

\arguments{
  \item{phideg}{Latitude}
  \item{lamdeg}{Longitude}
  \item{PROJ.DATA}{list of projection parameters}
}

\value{
 List
  \item{x}{x-coordinate}
  \item{y}{y-coordinate}
}
\references{Snyder, J. P., 1987; Map Projections - A Working Manual. U.S. Geological Survey Professional Paper 1395, 383 p.}
\author{Jonathan M. Lees<jonathan.lees.edu>}
\note{
  When calling the conversion from LL to XY or vice versa,
  convert the lon to 0 to 360.  Use RPMG::fmod for this conversion.
  This may be rectified in future revisions.
}
\seealso{setPROJ, GLOB.XY,  projtype,  utm.sphr.xy, UTMzone, plotUTM,
  utmbox, DATUMinfo}
\examples{
lat = 40.5
lon = -73.50
lon0 = -75
LON = RPMG::fmod(lon, 360)

wproj = setPROJ(type = 5, LAT0 = 0 , LON0 = lon0 , FE = 0 )

u1 = utm.elps.xy(lat, LON ,wproj ) 
utm.wgs84.xy(lat, LON ,wproj)

####  also for more general UTM:
###  this is the wgs84 projection
wproj1 = setPROJ(type = 8, LAT0 = 0 , LON0 = lon0 , FE = 0 , IDATUM=1 )
UTM.xy(lat, LON,wproj1)

###  this is the Clark-1866 (see page 270 in Snyder)
wproj8 = setPROJ(type = 8, LAT0 = 0 , LON0 = lon0 , FE = 0 , IDATUM=8)
UTM.xy(lat, LON,wproj8)

##  which is the same as:

uzone = UTMzone(lat, lon)

lon0 = uzone$CEN[2]
wproj = setPROJ(type = 5, LAT0 = 0 , LON0 = lon0 , FE = 500000 )
utm.elps.xy(lat, LON,wproj )


## to see all the Datums, use: DATUMinfo()


}


\keyword{misc}
