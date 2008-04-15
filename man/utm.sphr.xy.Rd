\name{utm.sphr.xy}
\alias{utm.sphr.xy}
\alias{equid.cyl.xy}
\alias{lambert.cc.xy}
\alias{lambert.ea.xy}
\alias{merc.sphr.xy}
\alias{stereo.sphr.xy}
\alias{utm.elps.xy}

\title{Map projection}
\description{
 Using Map projection parameters supplied and LAT-LON, return the x-y values
}
\usage{
utm.sphr.xy(phi, lam, PROJ.DATA)
}

\arguments{
  \item{phi}{Latitude}
  \item{lam}{Longitude}
  \item{PROJ.DATA}{list of projection parameters}
}

\value{
 List
  \item{x}{x-coordinate}
  \item{y}{y-coordinate}
}
\references{Snyder}
\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{GLOB.XY, setPROJ}

\keyword{misc}
