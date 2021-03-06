\name{rotmat2D}
\alias{rotmat2D}
\title{set a rotation matrix}
\description{
  set a rotation matrix
}
\usage{
rotmat2D(alph)
}
\arguments{
  \item{alph}{angle in radians }
}

\value{
matrix for rotation in 2 dimensions
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\examples{


########  make an ellipse
 theta=seq(0,360,by=5)*pi/180

r1 = 0.4
r2 = 0.2

   m=matrix(rep(0,2*length(theta)),ncol=2)
  
 m[,1]=r1*cos(theta)
    m[,2]=r2*sin(theta)


##  make a dummy plot and draw ellipse

 plot(c(0, 1), c(0, 1), main = "this is a test", sub = "sutitle",
        xlab = "this is x", ylab = "this is y")

lines(m[,1]+.5, m[,2]+.5)

## get rotation matrix
R = rotmat2D(32)

#########  apply rotation
nm=m \%*\% R


###  plot
lines(nm[,1]+.5, nm[,2]+.5, col='red')

}
\keyword{misc}
