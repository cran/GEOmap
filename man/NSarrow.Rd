\name{NSarrow}
\alias{NSarrow}
\alias{vane}
\title{North-South Weather Vane Arrow }
\description{Add north-south weather vane arrow figure 
}
\usage{
NSarrow(x = NULL, y = NULL, R = 1, col.arrow = 1, col.N = 1,
col.circ = 1, rot = 0, PMAT = NULL)
}
\arguments{
  \item{x}{X-location vector, if list, include both x and y values }
  \item{y}{Y-location vector, not needed if x is a list }
  \item{R}{radius, in plot coordinates }
  \item{col.arrow}{color for arrow, default="black" }
  \item{col.N}{color for N symbol }
  \item{col.circ}{color for circle }
  \item{rot}{rotation angle, degrees }
  \item{PMAT}{projection matrix, output of persp }
}
\details{
  The location list should have 2 values for x and y each, the second value for y
  determines the radius R if it is not provided.  The first element of y
  is the center of the weather vane. If no x-list is provided, the
  interactive locator function is invoked and a list is returned for future work.
}
\value{
  \item{x}{x-location}
  \item{y}{y-location}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{zebra}
\examples{

plot(c(1:10), c(1:10), type='n')

x=c(2,2)
y = c(8,9)

NSarrow(list(x=x, y=y))

##### move over and repeat, with rotation of 25 degrees west

x=c(5,5)
y = c(8,9)

NSarrow(list(x=x, y=y), rot=25)




}
\keyword{misc}
