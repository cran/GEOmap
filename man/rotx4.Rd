\name{rotx4}
\Rdversion{1.1}
\alias{rotx4}

\title{x-axis rotation matrix}

\description{x-axis rotation matrix}
\usage{
rotx4(vec)
}
\arguments{
  \item{vec}{vector of direction cosines
}
}
\details{
   Length of vector cannot be zero.
}
\value{Matrix for rotation
  
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{roty4, rotdelta4
}
\examples{

v = c(12,13,-4)

rotx4(v)



}
\keyword{misc}
