\name{getnicetix}
\alias{getnicetix}
\title{Nice Looking Lat-Lon pairs for plotting}
\description{
  Given a set of lat lon pairs, return a new set of tic marks
}
\usage{
getnicetix(lats, lons)
}
\arguments{
  \item{lats}{latitude range }
  \item{lons}{longitude range }
}

\value{
  \item{LAT}{list output of niceLLtix}
  \item{LON}{list output of niceLLtix}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{niceLLtix}
\examples{
proj = setPROJ(7, LAT0 = 0 , LON0= -93)
rx = c(652713.4, 656017.4)
ry = c(1629271, 1631755)

   gloc = XY.GLOB(rx, ry, proj)
    
    G = getnicetix(gloc$lat, gloc$lon)
 
print(G)





}
\keyword{misc}
