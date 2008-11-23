\name{printGEOinfo}
\alias{printGEOinfo}
\title{printGEOinfo }
\description{
  Print information on GEOmap strokes
}
\usage{
printGEOinfo(MAP, kstroke)
}
\arguments{
  \item{MAP}{GEOmap }
  \item{kstroke}{index to strokes }
}
\details{
  Prints some of the meta data stored in the GEOmap header list, strokes.
}
\value{
 Side Effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{printGEOmap}
\examples{

data(coastmap)
printGEOinfo(coastmap, 1:10)

}
\keyword{misc}
