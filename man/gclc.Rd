\name{gclc}
\alias{gclc}
\alias{GCLCFR}

\title{Global to local coordinates}
\description{
  OLD projection sometimes used in Lees' tomography. No need for
  projection data, it is included in the code.
}
\usage{
gclc(phiorg, lamorg, phi, lam)
}

\arguments{
  \item{phiorg}{lat origin}
  \item{lamorg}{lon origin}
  \item{phi}{lat}
  \item{lam}{lon}
}
\details{
 This may be defunct now.
}
\value{
 
  \item{x}{coordinate, km}
  \item{y}{coordinate, km}

}
\note{Orignally from R. S. Crosson}
\author{Jonathan M. Lees<jonathan.lees.edu>}
\seealso{lcgc}

\examples{
gclc(23, 35, 23.5, 35.6)
}

\keyword{misc}
