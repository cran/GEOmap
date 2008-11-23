\name{xyz2ll}
\Rdversion{1.1}
\alias{xyz2ll}
\title{Cartesian to Lat-Lon
}
\description{Cartesian to Lat-Lon
}
\usage{
xyz2ll(x)
}
\arguments{
  \item{x}{3-vector}
}
\details{ Returns Latitude not Co-latitude
}
\value{ 2-vector of lat-lon
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{ Does only one point at a time
}


\seealso{Lxyz2ll}
\examples{
xyz2ll(c(1,1,1) )


}
\keyword{misc}
