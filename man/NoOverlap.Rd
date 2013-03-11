\name{NoOverlap}
\Rdversion{1.1}
\alias{NoOverlap}
\title{Shift Symbols 
}
\description{Shift Symbols such that there is no overlap
}
\usage{
NoOverlap(x, y, focsiz, SEL = 0, OLDx = 0, OLDy = 0, cenx = 0, ceny = 0)
}
\arguments{
  \item{x}{x-location
}
  \item{y}{y-location
}
  \item{focsiz}{symbol size
}
  \item{SEL}{selection of which symbols to shift
}
  \item{OLDx}{x-locations of origin
}
  \item{OLDy}{y-locations of origin
}
  \item{cenx}{center x
}
  \item{ceny}{center y
}
}
\details{
  Program is used for finding positions for exploding.
  A vector is dcalculated from each origin to each point and explosions
  are projected along these directions until a position is found that
  does not overlap.
  The position is nudged by a value of focsiz at each step.
  If OLDx and OLDy are not provided,
  cenx and ceny are used as origin points.
}
\value{x,y list of new positions
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{ExplodeSymbols
}
\examples{
 draw.circ<-function (x, y, r, ...) 
      {
        CI = RPMG::circle(1)
        for (i in 1:length(x)) {
          Cx = x[i] + r * CI$x
          Cy = y[i] + r * CI$y
          lines(c(Cx, Cx[1]), c(Cy, Cy[1]), type = "l", ...)
        }
      }
 

 x = rnorm(20)
    y = rnorm(20)

    rx = range(x)
    ry = range(y)

    drx = diff(rx)
    dry = diff(ry)
    XPCT=.2
    rx = c(rx[1]-XPCT*drx, rx[2]+XPCT*drx)
    ry = c(ry[1]-XPCT*dry, ry[2]+XPCT*dry)



   plot(rx , ry , type='n', asp=1, xlab="km", ylab="km")

    u = par("usr")

focsiz = 0.04* (u[2]-u[1])

   draw.circ(x, y, focsiz, col='red')
 NXY = NoOverlap(x,y,focsiz)

 plot(rx , ry , type='n', asp=1, xlab="km", ylab="km")

    u = par("usr")

focsiz = 0.04* (u[2]-u[1])


draw.circ(NXY$x, NXY$y, focsiz, col="blue" )

    segments(x,y,NXY$x, NXY$y)
   

}
\keyword{misc}
