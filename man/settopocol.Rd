\name{settopocol}
\alias{settopocol}

\title{Topographic Color Map}
\description{
  Set up vectors and structures for creating a
  color map for topographic plots
}
\usage{
settopocol()
}

\details{
  RGB Colors are defined for topographic
  elevations and/or depths.  The basic data is stored
  as z1 red1 green1 blue1 z2 red2 green2 blue2
  and linear interpolation is used between
  elevations.
  The color set here extends from green in lowlands around sealevel
  through browns and light-browns through to
  whites at snow covered peaks.
  
  
}
\value{
  LIST:calcol=calcol , coltab=coltab
  \item{calcol}{list(z1, r1,g1,b1,
     z2,  r2,g2,b2, note)}
  \item{coltab}{color table, matrix }
 
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\examples{
settopocol()
}

\keyword{misc}
