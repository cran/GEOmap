\name{rectPERIM}
\alias{rectPERIM}
\title{Extract a rectangular perimeter
}
\description{Extract a rectangular perimeter
}
\usage{
rectPERIM(x, y = 1, pct = 0)
}
\arguments{
  \item{x}{x values or a list include x, y members
}
  \item{y}{y values, if missing, x must be a list
}
\item{pct}{Percent expansion, based on range of x and y values.
  If pct>1 it is divided by 100 to get a fractional percent expansion.
}
}
\details{The rectangular box will be expanded
  based on the percent pct. 
}
\value{ list of x, y values from lower left corner counter clockwise
  around perimeter
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{getGEOperim
}
\examples{

fx  =rnorm(20)
fy = rnorm(20)

plot(fx, fy, xlim=c(-4, 4), ylim=c(-4,4))
rp = rectPERIM(fx, fy)

polygon(rp)
text(rp, labels=1:4, pos=c(1,1,3,3), font=2, cex=2)


fx2  =rnorm(20, m=-1)
fy2 = rnorm(20, m=-1)

Fx = list(x=fx2, y=fy2)

points(Fx$x, Fx$y, col='red')

rp = rectPERIM(Fx)

polygon(rp, border='red')


########   try expanding the perim:

plot(fx, fy, xlim=c(-4, 4), ylim=c(-4,4), asp=1)
rp = rectPERIM(fx, fy, pct=0.1)

polygon(rp)

rp = rectPERIM(fx, fy, pct=0.2)

polygon(rp)



}
\keyword{misc}
