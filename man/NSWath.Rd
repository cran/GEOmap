\name{NSWath}
\alias{NSWath}
\docType{data}
\title{Cross sectional Swaths of Earthquakes over Japan}
\description{
  Set of 4 swaths for cross section across Japan
}
\usage{data(NSWath)}
\format{
  list of cross sections each conists of a list of form:
  \describe{
\item{r}{r-distance along cross section (x-coordinate)}
  \item{dh}{distance from cross seection}
  \item{depth}{depth in cross section (y-coordinate) }
  \item{flag}{index vector of which earthquakes fell in swath and depth range}
  \item{InvBox}{coordinates of swath for plotting on map}
  }
}


\source{
  Data is extrcted from an earthquake data base of relocated events
  provided by Robert Engdahl.
}
\references{
 Engdahl, E. R., R. D. van der Hilst, S. H. Kirby, G. Ekstrom, K. M. Shedlock, and A. F. Sheehan (1998), A global survey of slab structures and internal processes using a combined data base of high-resolution earthquake hypocenters, tomographic images and focal mechanism data, Seismol. Res. Lett., 69, 153-154. 
}

\examples{
\dontrun{
data(NSWath)
for(i in 1:length(NSWath))
{
dev.new()
LAB = attr(NSWath[[i]], "LAB")

XSECwin( NSWath[[i]] , iseclab=i, xLAB=LAB , labs=NULL, demo=TRUE  )   
}

}

}
\keyword{datasets}
