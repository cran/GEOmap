\name{gmat}
\Rdversion{1.1}
\alias{gmat}
\title{Globe Rotation Matrix
}
\description{Globe Rotation Matrix
}
\usage{
gmat(vec, p, alpha)
}
\arguments{
  \item{vec}{ vector axis to rotate about
}
  \item{p}{ translation point (c(0,0,0))
}
  \item{alpha}{ angle to rotate, degrees 
}
}
\details{
  Given an arbitrary axis, return matrix for rotation about the
  axis by alpha degrees.
}
\value{4 by 4 Matrix for translation and rotation
}
\references{Rogers and Adams
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{


################   kamchatka

kamlat = c(48.5,  65)
kamlon = c(150, 171)

KAMLAT0=mean(kamlat)
KAMLON0=mean(kamlon)


################  korea

 KORlon = c(123,133)
    KORlat = c(33,44)


KORLON0=mean(KORlon)
KORLAT0=mean(KORlat)

# convert to cartesian
v1 = ll2xyz(KORLAT0, KORLON0 )
v2 = ll2xyz(KAMLAT0,  KAMLON0)

###   get cross product
g = X.prod((v1), (v2))

### use dot product to get angle
delta = (180/pi)*acos( sum(v1*v2)/(sqrt(sum(v1^2))*sqrt(sum(v2^2))))

###   get rotation matrix
R1 =gmat(g, c(0,0,0) , -delta)


}
\keyword{misc}
