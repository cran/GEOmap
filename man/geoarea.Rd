\name{geoarea}
\alias{geoarea}

\title{Area of Map objects}
\description{
  vector of areas of polygons in map
}
\usage{
geoarea(MAP, proj, ncut=10)
}

\arguments{
  \item{MAP}{Map structure}
  \item{proj}{projection}
  \item{ncut}{minimum number of points in polygon}
  
}
\details{
  Uses splancs function
}
\value{
 vector of areas
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
  areas smaller than a certain tolerance are NA
}


\keyword{misc}
