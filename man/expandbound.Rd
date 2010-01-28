\name{expandbound}
\alias{expandbound}
\title{Expand Bounds
}
\description{Calculate an
  expanded bounding region based on a percent
  of the existing boundaries
}
\usage{
expandbound(g, pct = 0.1)
}
\arguments{
  \item{g}{vector of values
}
  \item{pct}{fractional percent to expand
}
}
\details{
  uses the range of the exising
  vector to estimate the expanded bound
}
\value{
  vector, new range
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{

i = 5:10
exi = expandbound(i, pct = 0.1)
range(i)
range(exi)

}
\keyword{misc}
