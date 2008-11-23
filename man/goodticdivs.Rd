\name{goodticdivs}
\alias{goodticdivs}
\title{Nice tic division}
\description{
  Determine a reasonable tick division for
  lat-lon tic marks.
}
\usage{
goodticdivs(ddeg)
}
\arguments{
  \item{ddeg}{degree differnce}
}

\value{
  \item{K}{suggested divisor}
 
}
\details{
  Designed to give approximately 4-6 divisions
  for plotting given the range input.
  }

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{niceLLtix}
\examples{

goodticdivs(20)
goodticdivs(100)



}
\keyword{misc}
