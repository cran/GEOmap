\name{utmbox}
\alias{utmbox}

\title{Get UTM Box info  }
\description{
  Get UTM Box info 
}
\usage{
utmbox(lat, lon)
}

\arguments{
  \item{lat}{latitude}
  \item{lon}{longitude}
}

\value{
  List:
  \item{lon}{input  point  longitude}
  \item{lat}{input point latitude}
  \item{LON}{LL corner longitude}
  \item{LAT}{LL corner latitude}
  \item{utmbox}{List: x=utm number, y=utm letter }
  \item{UTM0}{List: center of box: lam=long, phi=lat}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{plotUTM}
\examples{


lat = 35.76658
lon = 279.4335
utmbox(lat, lon)  

}


\keyword{misc}

