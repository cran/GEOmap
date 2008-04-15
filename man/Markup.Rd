\name{Markup}
\alias{Markup}
\title{Add markup information to an existing plot}
\description{
  For use in GEOmap to add labels to a geographic plot
}
\usage{
Markup(MM = list(), sel = 1, cex = 1, ...)

}
\arguments{
  \item{MM}{list of markup infromation}
  \item{sel}{vector, select which marks to be plotted}
  \item{cex}{character expansion}
  \item{\dots}{graphical parameters for par}
}
\details{ Uses the locator function
}
\value{
 Graphical Side Effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{setMarkup, plotGEOmapXY}
\examples{

\dontrun{
dumplot()
LABS = c("this is", "a", "test")

MUP = setMarkup(LABS)
}


}
\keyword{misc}
