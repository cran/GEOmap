\name{setPROJ}
\alias{setPROJ}

\title{Set Projection}
\description{
Setup parameters for Map Projection
}
\usage{
setPROJ(type = 1, LAT0 = 0, LON0 = 0, LAT1 = 0, LAT2 = 0, LATS = NULL, LONS = NULL, DLAT = NULL, DLON = NULL, FE = 0, FN = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{Type of projection}
  \item{LAT0}{Central Latitude}
  \item{LON0}{Central Longitude}
  \item{LAT1}{Latitude parameter for special projection, where needed}
  \item{LAT2}{Latitude parameter for special projection, where needed}
  \item{LATS}{vector of range of Latitudes}
  \item{LONS}{vector of range of Longitudes}
  \item{DLAT}{difference of Lats}
  \item{DLON}{difference of Lons}
  \item{FE}{False Easting}
  \item{FN}{False Northing}
}
\details{
  Set up for the various projections used by GEOmap
}
\value{
List of values described above
}
\references{
  Snyder, John P., Map Projections- a working manual, USGS, Professional Paper, 1987.

}
\author{Jonathan M. Lees<jonathan.lees.edu>}
\note{
  Some of the parameters are not critical to all the
  choices or Map Projection. In that case they are set to defaults and
  ignored by that projection.

  LONs are modified and rectified by fmod function.

}
\seealso{projtype, XY.GLOB, GLOB.XY}
\examples{

######  type
projtype()
######  type = mercator spherical
setPROJ(type = 1, LAT0 =23, LON0 = 35)

### Hengill Map: lambert.cc
setPROJ(type=3, LAT0=65, LON0=360-19 ,LAT1=64+15/60,  LAT2=65+45/60,LATS=, LONS=NULL, DLAT=NULL, DLON=NULL,FE=500000,FN=500000)

### old lees/crosson projection
setPROJ(type=99, LAT0=23, LON0=35, LATS=NULL, LONS=NULL, DLAT=NULL,
DLON=NULL, FN =0)

###  world map equid.cyl
setPROJ(6, LAT0=0, LON0=0)

##  North Carolina Map lambert.cc
setPROJ(type=3, LAT0=36+20/60, LON0=78+30/60,LAT1=36+46/60,
LAT2=37+58/60, LATS=NULL, LONS=NULL, DLAT=NULL, DLON=NULL,FE=0,FN=0)

###  No Projection
setPROJ(type = 0, LAT0 =23, LON0 = 35)


}

\keyword{misc}
