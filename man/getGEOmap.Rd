\name{getGEOmap}
\alias{getGEOmap}

\title{Get Geomap}
\description{
  Get Geomap from ascii files
}
\usage{
getGEOmap(fn)
}

\arguments{
  \item{fn}{root name}
}
\details{
  Files are stored as a pair:  rootname.strks and rootname.pnts
}
\value{
  \item{STROKES}{List of stroke information:}
  \item{nam}{name of stroke}
  \item{num}{number of points}
  \item{index}{index where points start}
  \item{col}{color}
  \item{style}{plotting style: 1=point, 2=line,3=polygon}
  \item{code}{character, geological code}
  \item{LAT1}{bounding box lower left Lat}
  \item{LAT2}{bounding box upper right Lat}
  \item{LON1}{bounding box lower left Lon}
  \item{LON2}{bounding box upper right  Lon}
  \item{POINTS}{List of point LL coordinates, list(lat, lon)}
  \item{PROJ}{optional projection parameters}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{plotGEOmapXY, boundGEOmap}
\examples{

\dontrun{
library(geomapdata)

 data(cosomap)
     data(faults)
     data(hiways)
     data(owens)

cosogeol = getGEOmap("/home/lees/XMdemo/GEOTHERM/cosogeol")

cosogeol = boundGEOmap(cosogeol)


 proj = cosomap$PROJ
    
plotGEOmapXY(cosomap, PROJ=proj,  add=FALSE, ann=FALSE, axes=FALSE)
  
 plotGEOmapXY(cosogeol, PROJ=proj,  add=TRUE, ann=FALSE, axes=FALSE)
  

  plotGEOmapXY(cosomap, PROJ=proj,  add=TRUE, ann=FALSE, axes=FALSE)
  
  plotGEOmapXY(faults, PROJ=proj,  add=TRUE, ann=FALSE, axes=FALSE)
  
}

}


\keyword{misc}

