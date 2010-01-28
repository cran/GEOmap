\name{explode}
\Rdversion{1.1}
\alias{explode}
\title{Explode Points
}
\description{Explode a set of points away from a center point
}
\usage{
explode(fxy, dixplo=1, mult=1, cenx=0, ceny=0, PLOT=FALSE)
}
\arguments{
  \item{fxy}{list of x, y coordinates
}
  \item{dixplo}{distance to explode
}
  \item{mult}{multiplier for the distance
  }
  \item{cenx}{x coordinate center of explosion 
  }
  \item{ceny}{y  coordinate center  of explosion 
  }
  \item{PLOT}{logical, TRUE=make a plot of the resulting explosion
  }
     

  
}
\details{
  If cenx and ceny is missing it is assumed to
  be the mean of the coordinates.
  Program  calculates the new locations
  radiating away from the central point.
  No protection against overlapping symbols is included.
  
}
\value{list of new x,y values
   \item{x}{new x coordinates 
  }
  \item{y}{new y coordinates 
  }
    
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{ExplodeSymbols, NoOverlap
}
\examples{


############  random data
x = rnorm(20)
y = rnorm(20)

NEW = explode(list(x=x,y=y), dixplo =1)

plot(range(c(x,NEW$x)), range(c(y,NEW$y)), asp=1, type='n')
segments(x,y,NEW$x, NEW$y)
points(x,y, pch=3, col='red')
points(NEW$x, NEW$y, pch=6, col='blue', cex=2)


###  try a larger radius:
NEW2 = explode(list(x=x,y=y), dixplo =1.3)
points(NEW2$x, NEW2$y, pch=7, col='brown', cex=2, xpd=TRUE)
arrows(NEW$x, NEW$y,NEW2$x, NEW2$y, col='green' )



#####   try with a different center
cenx=-1; ceny=-1
NEW = explode(list(x=x,y=y), dixplo =1, cenx=cenx, ceny=ceny)
plot(range(c(x,NEW$x)), range(c(y,NEW$y)), asp=1, type='n')
points(x,y, pch=3, col='red')
segments(x,y,NEW$x, NEW$y)
points(NEW$x, NEW$y, pch=6, col='blue', cex=2)
points(cenx, ceny, pch=8, col='purple')
text(cenx, ceny, labels="Center Point", pos=1)



}
\keyword{misc}
