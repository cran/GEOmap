\name{DATUMinfo}
\alias{DATUMinfo}
\title{Datum information.
}
\description{
  Return a small data base of
  Datum values for use in UTM projections.
}
\usage{
DATUMinfo()
}
\details{
  The function just return a list with the relavent information.
}
\value{
  List:
  \item{Datum}{character name}
   \item{Equatorial Radius, meters (a)}{numeric}
  \item{Polar Radius, meters (b)}{numeric}
  \item{Flattening (a-b)/a}{numeric}
  \item{Use}{character  usage}
}
\references{
  websource = \url{http://www.uwgb.edu/dutchs/UsefulData/UTMFormulas.htm}
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{UTM.xy, UTM.ll, setPROJ
}
\examples{

h = DATUMinfo()
data.frame(h)

}
\keyword{misc}
