\name{EHB.LLZ}
\alias{EHB.LLZ}
\docType{data}
\title{Earthquake Location Data}
\description{
  Global Earthquake catalog locations from Engdahl, et al.
}
\usage{data(EHB.LLZ)}
\format{
  \describe{
 \item{lat}{Latitude}
  \item{lon}{Longitude}
  \item{z}{depth in km}
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
data(EHB.LLZ)
## maybe str(EHB.LLZ) ; plot(EHB.LLZ) ...
}
\keyword{datasets}
