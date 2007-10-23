\name{setMarkup}
\alias{setMarkup}
\title{Set up mark up for maps}
\description{
  Interactive set up of mark of labels for a map
}
\usage{
setMarkup(LABS = NULL, PROJ = NULL)
}
\arguments{
  \item{LABS}{vector of labels}
  \item{PROJ}{projection structure}
 
}
\details{
  labels are set one-by-one and the user inout relevant information
  like locator() and other features
}
\value{
  List of Markup information
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{Markup}
\examples{

\dontrun{
dumplot()
LABS = c("this is", "a", "test")

MUP = setMarkup(LABS)
}

}
\keyword{misc}
