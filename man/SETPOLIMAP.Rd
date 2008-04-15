\name{SETPOLIMAP}
\alias{SETPOLIMAP}

\title{Set up polygons for World map Database}
\description{
 Divides world into continents.
}
\usage{
SETPOLIMAP()
}

\details{
  Used for CIA data base 
}
\value{
  Returns GEOmap list of continents
  \item{STROKES}{list(nam, num, index, col, style, code, LAT1, LAT2, LON1, LON2)}
  \item{POINTS}{list(lat, lon)}
  \item{PROJ}{list(type, LAT0, LON0, LAT1, LAT2, LATS, LONS, DLAT, DLON, FE, FN, name)}


  
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{selectPOLImap}
\examples{
LMAP = SETPOLIMAP()
}

\keyword{misc}
