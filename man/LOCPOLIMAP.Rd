\name{LOCPOLIMAP}
\alias{LOCPOLIMAP}

\title{LOCPOLIMAP}
\description{
 This program takes a point and return the continent index for database manipulation. 
}
\usage{
LOCPOLIMAP(P, MAP)
}

\arguments{

  \item{P}{Point selected on screen using locator}
  \item{MAP}{List of maps and coordinates from database }
}
\details{
  Uses the CIA data base definitions.
}
\value{
 
  \item{J}{Index to map data base}

}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\seealso{SETPOLIMAP}
\examples{

P = list(lat=36.09063, lon=19.44610)

LMAP = SETPOLIMAP()

 J = LOCPOLIMAP(P, LMAP)
J

}

\keyword{misc}
