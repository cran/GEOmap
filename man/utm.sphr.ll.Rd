\name{utm.sphr.ll}
\alias{utm.sphr.ll}
\alias{equid.cyl.ll}
\alias{lambert.cc.ll}
\alias{lambert.ea.ll}
\alias{merc.sphr.ll}
\alias{stereo.sphr.ll}
\alias{utm.elps.ll}
\alias{utm.wgs84.ll}

\title{Map projection}
\description{
 Using Map projection parameters supplied and X-Y, return the LAT-LON values
}
\usage{
utm.sphr.ll(x , y , PROJ.DATA)
}

\arguments{
  \item{x}{x}
  \item{y}{y}
  \item{PROJ.DATA}{list of projection parameters}
}

\value{
 List
  \item{phi}{Latitude-coordinate}
  \item{lam}{Longitude-coordinate}
}
\references{Snyder}
\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{GLOB.XY, setPROJ}

\keyword{misc}
