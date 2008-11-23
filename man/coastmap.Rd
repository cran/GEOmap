\name{coastmap}
\alias{coastmap}
\docType{data}
\title{Global Coast Map}
\description{
 Global Maps of Coast 
}
\usage{data(coastmap)}
\format{
  List structure:
  \describe{
  \item{STROKES}{list(nam, num, index, col, style, code, LAT1, LAT2, LON1, LON2)}
  \item{POINTS}{list(lat, lon)}
  \item{PROJ}{list(type, LAT0, LON0, LAT1, LAT2, LATS, LONS, DLAT, DLON, FE, FN, name)}
 }
}
\details{
This map list is used for filling in coastal lines for global maps.
The style=3 is for filling in polygons.  The strokes are named
for easier access to particular parts ofthe globe.
Asia and Africa are one stroke, as are North and South America.
there are currently three codes: C=major coast,  c=smaller coasts,
L=interior lakes.
 }


\examples{
data(coastmap)
#######  see the codes:
unique(coastmap$STROKES$code)
#########  see the different names:
unique(coastmap$STROKES$nam)

#########  change the colors based on code
coastmap$STROKES$col[coastmap$STROKES$code=="C" ] = rgb(1, .6, .6)
coastmap$STROKES$col[coastmap$STROKES$code=="c" ] = rgb(1, .9, .9)
coastmap$STROKES$col[coastmap$STROKES$code=="L" ] = rgb(.6, .6, 1)

plotGEOmap(coastmap , border='black' , add=FALSE, xaxs='i')





##
}
\keyword{datasets}
