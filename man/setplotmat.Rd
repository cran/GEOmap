\name{setplotmat}
\alias{setplotmat}

\title{set up matrices for selecting from eTOPO5}
\description{
set up matrices for selecting from eTOPO5
}
\usage{
setplotmat(x, y)
}

\arguments{
  \item{x}{vector of lons}
  \item{y}{vector of lats}
}
\details{
 For extracting from ETOPO5 and ETOPO2, used internally in DOTOPOMAPI
}
\value{
list(x=EX, y=WHY)
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{DOTOPOMAPI}
\examples{
PLOC= list(LON=c(138.3152, 139.0214), 
LAT=c(35.09047, 35.57324))

ax = seq(from=PLOC$LON[1],   to=PLOC$LON[2], length=10)
ay = seq(from=PLOC$LAT[1],   to=PLOC$LAT[2], length=10)

 G = setplotmat(ax,ay)

}

\keyword{misc}
