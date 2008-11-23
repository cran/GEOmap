\name{dms}
\alias{dms}
\title{Convert decimal degrees to degree, minutes, seconds}
\description{Convert decimal degrees to degree, minutes, seconds
}
\usage{
dms(d1)
}
\arguments{
  \item{d1}{decomal degrees }
}

\value{
  list
  \item{d}{degrees}
  \item{m}{minutes}
  \item{s}{seconds}
  
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\examples{

dms(33.12345)

H = dms(-91.8765)

print(H)

newH = H$d+H$m/60+H$s/3600
print(newH)


}
\keyword{misc}
