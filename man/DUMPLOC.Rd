\name{DUMPLOC}
\alias{DUMPLOC}

\title{DUMP vectors to screen in list format}
\description{
  For saving vectors to a file after the locator function
  has been executed.
}
\usage{
DUMPLOC(zloc, dig = 12)
}

\arguments{
  \item{zloc}{x,y list of locator positions}
  \item{dig}{number of digits in output}
}

\value{
Side effects: print to screen
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}


\examples{
G=list()
G$x=c(-1.0960,-0.9942,-0.8909,-0.7846,-0.6738,-0.5570,-0.4657,-0.3709,
-0.2734,-0.1740,-0.0734, 0.0246, 0.1218, 0.2169, 0.3086, 0.3956, 0.4641, 
0.5293, 0.5919, 0.6530, 0.7131)
G$y=c(-0.72392,-0.62145,-0.52135,-0.42599,-0.33774,-0.25896,-0.20759,
-0.16160,-0.11981,-0.08105,-0.04414,-0.00885, 0.02774, 0.06759, 0.11262, 
0.16480, 0.21487, 0.27001, 0.32895, 0.39044, 0.45319)

g = PointsAlong(G$x, G$y, N=3)
DUMPLOC(g, dig = 5)


}

\keyword{misc}
