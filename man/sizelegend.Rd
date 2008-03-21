\name{sizelegend}
\alias{sizelegend}
\title{Magnitude size legend}
\description{
  Plot a simple legend of magnitude sizes at the top of a plot.
}
\usage{
sizelegend(se, am, pch = pch)
}
\arguments{
  \item{se}{vector, sizes}
  \item{am}{vector, labels}
  \item{pch}{plotting character}
}
\details{
  A box around the legend is currently introduced.
}
\value{
 Graphical Side Effect
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}


\examples{


x = rnorm(30)
y = rnorm(30)

mags = runif(30, 1,8)


plot(x, y, type="n")

esiz = exp(mags)
rsiz = RESCALE(esiz, .4, 10, min(esiz), max(esiz))
points(x, y, pch=1, cex=rsiz)

am = pretty(mags)
am = am[am>min(mags) & am<max(mags) ]

em = exp(am)
se = RESCALE(em, .4, 10, min(esiz), max(esiz))

sizelegend(se, am, pch=1)



}
\keyword{misc}
