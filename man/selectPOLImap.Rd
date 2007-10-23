\name{selectPOLImap}
\alias{selectPOLImap}

\title{Select Geographic map from database }
\description{
  Given two indexes, one of a continent the other of
  map information, select the database
}
\usage{
selectPOLImap(J, K)
}

\arguments{
  \item{J}{Index to continents}
  \item{K}{Index to styles}
}
\details{

 J values refer to one of:  c("africa", "asia",  "europe", "namer", "samer")
  K values refer to one of: c("cil", "riv", "bdy")
 
}
\value{
  GEOmap structure, with the lines colored according to the (J, K) choice
}

\author{Jonathan M. Lees<jonathan.lees.edu>}

\examples{


###  for south america rivers 
ZMAP = selectPOLImap(3,2)


plotGEOmap(ZMAP)

}

\keyword{misc}
