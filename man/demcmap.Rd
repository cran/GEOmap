\name{demcmap}
\alias{demcmap}

\title{Color Map from DEM}
\description{
  create a color map from a DEM (Digital Elevation Map)
}
\usage{
demcmap(ZTOPO, n = 100, ccol = NULL)
}

\arguments{
  \item{ZTOPO}{Topography structure}
  \item{n}{number of colors}
  \item{ccol}{color structure}
}

\value{
  vector of rgb colors
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{rgb, settopocol}

\keyword{misc}
