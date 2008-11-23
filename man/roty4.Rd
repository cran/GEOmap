\name{roty4}
\Rdversion{1.1}
\alias{roty4}
\title{y-axis rotation matrix
}
\description{y-axis rotation matrix
}
\usage{
roty4(vec)
}
\arguments{
  \item{vec}{vector of direction cosines}
}
\details{
  Length of vector cannot be zero.
}
\value{Matrix for rotation
}
\references{Rogers and Adams}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{rotx4, rotdelta4}
\examples{

v = c(12,13,-4)

roty4(v)

}
\keyword{misc}
