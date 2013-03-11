\name{plotusa}
\alias{plotusa}

\title{Map of USA}
\description{
 Quick plot of USA project with UTM.
}
\usage{
plotusa(USAmap, LATS=c(22,49.62741), LONS=c(229.29389,296.41803), add=FALSE)
}

\arguments{
   \item{USAmap}{Map for the U.S. (from geomapdata) } 
  \item{LATS}{vector of latitude bounds} 
  \item{LONS}{vector of longitude bounds}
  \item{add}{  add to existing plot }

  
}

  
\value{
 Graphical Side Effect
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{zebra}
\examples{

\dontrun{
library(geomapdata)
data(package='geomapdata', "USAmap")
plotusa(USAmap)

}
}

\keyword{hplot}
