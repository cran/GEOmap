\name{Ellipsoidal.Distance}
\alias{Ellipsoidal.Distance}
\title{Ellipsoidal Distance
}
\description{Ellipsoidal Distance given Latitude and Longitude
}
\usage{
Ellipsoidal.Distance(olat, olon, tlat, tlon, a = 6378137, b = 6356752.314, tol=10^(-12))
}
\arguments{
  \item{olat}{Origin Latitude, degrees
}
  \item{olon}{Origin Longitude, degrees
}
  \item{tlat}{Target Latitude, degrees
}
  \item{tlon}{Target Longitude, degrees
}
  \item{a}{major axis, meters.  If missing uses the 
}
  \item{b}{minor axis, meters
  }
  \item{tol}{Tolerance for convergence, default=10^(-12)
  }

  
}
\details{
Uses Vincenty's formulation to calculate the distance along a great
circle on an ellipsoidal body.

If a and be are not provided,
they are set by default to a=6378137.0 , b=6356752.314,
the WGS-84 standard.

Only one pair of (olat, olon) and (tlat, tlon) can
be given at a time.  The program is not vectorized.

Quoting from the wiki page this algorithm was extracted from:

"Vincenty's formulae are two related iterative methods used in
   geodesy to calculate the distance
  between two points on the surface of an spheroid, developed by
  Thaddeus Vincenty in 1975. They are based on the assumption that
              the figure of the Earth is an oblate
   spheroid, and hence are more accurate than methods such as
    great-circle distance which assume a spherical Earth.

  The first (direct) method computes the location of a point which is a given
  distance and azimuth (direction) from another point. The second (inverse) method
 computes the geographical distance and azimuth between two given points.
 They have been widely used in geodesy because they are
  accurate to within 0.5 mm (.020 sec)  on the Earth ellipsoid"


  
}
\value{
  list
  \item{dist}{distance, km}
  \item{az}{azimuth, degrees}
  \item{revaz}{reverse azimuth, degrees}
  \item{err}{=0, if convergence failed, else=1}
  
}

\note{
  
  Latitudes >90 and < -90 are not allowed.  NA's are returned.

  If points are identical, a distance of zero is returned
  and NA for the azimuths.
  If there is some problems with convergence or division by
  zero, NA's are returned and error message is printed.

  A couple of known cases that do not work are, e.g.:
  (olat=0; olon=0; tlat=0; tlon=-180)  and 
  (olat=0; olon=0; tlat=0; tlon=180).
  They will return NA's to avoid division by zero.
  
  I am not sure how to deal with these cases yet.

  
  The reverse azimuth is the angle from the meridian
  on the target point to the great circle from the origin to the target
  (as far as I can tell).  If distaz and Ellipsoidal.Distance
  are compared, they give the same azimuth, and the
  absolute angles of baz (from distaz) and revaz (from
  Ellipsoidal.Distance) will add to 180 degrees.

}



\references{

  http://en.wikipedia.org/wiki/Vincenty\%27s_formulae

  Vincenty, T. (April 1975). Direct and Inverse Solutions of Geodesics
  on the Ellipsoid with application of nested equations.
  Survey Review XXIII (misprinted as XXII) (176): 88.201393.
  http://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf. Retrieved 2009-07-11.

}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{distaz
}
\examples{

####    compare to spheroidal calculation distaz
####


R.MAPK = 6378.2064
N =20

OUT = list(dadist=0, ed2dist=0, ed1dist=0, dif2=0, dif1=0, pct1=0)
for( i in 1:N)
  {

    olat = runif(1, -90, 90)
    olon = runif(1, 0, 180)

     tlat = runif(1, -90, 90)
    tlon = runif(1, 0, 180)

##########  older spherical calculation
    da = distaz(olat, olon, tlat, tlon)
#####  ed1 = elliposidal earth
    ed1 = Ellipsoidal.Distance(olat, olon, tlat, tlon)
#####   ed2 spherical earth using
############      ellipsoidal calculations, compare with
distaz
   ed2 =  Ellipsoidal.Distance(olat, olon, tlat, tlon, a=R.MAPK*1000, b=R.MAPK*1000)

    dif1 =  da$dist-ed1$dis
    dif2 =  da$dist-ed2$dis

    pct1 = 100*dif1/ed1$dist

##############   OUT = format( c(da$dist, ed2$dist, ed1$dist, dif2, dif1, pct1) , digits=10)

    OUT$dadist[i] =da$dist
    OUT$ed2dist[i] =ed2$dist
OUT$ed1dist[i]=ed1$dist
OUT$dif2[i]= dif2
OUT$dif1[i]=dif1
OUT$pct1[i]=pct1

###cat(paste(collapse=" ", OUT), sep="\n")

  }


print( data.frame(OUT) )



###############   some extreme cases can cause problems
#######  here compare  Ellipsoidal.Distance with spherical program distaz

Alat = c(90,   90,  90,   90,  45,  45,  45,  45,   0,    0,    0, 0)
Alon = c(180, 180,-180, -180,  45,  45,  45,  45,   0,    0,    0, 0)
Blat = c(-90, -45,   0,   45, -45,  0,    0,  -80,  45,   0,    0, 0)
Blon = c(180,-180, 180,    0, -45,  0, -180,  100, -60, -180, 180, 0)


BOUT = list(olat=0, olon=0, tlat=0, tlon=0, dadist=0, ed2dist=0, daaz=0, ed2az=0, dabaz=0, ed2baz=0)

R.MAPK = 6378.2064
for(i in 1:length(Alat))
{

  olat = Alat[i]
  olon  = Alon[i]
  tlat  = Blat[i]
  tlon  = Blon[i]

 da = distaz(olat, olon, tlat, tlon)
  ed2 = Ellipsoidal.Distance(olat, olon, tlat, tlon, a=R.MAPK*1000, b=R.MAPK*1000)
 cat(paste("i=", i), sep="\n")
 

 BOUT$olon[i] =olon
 BOUT$olat[i] =olat
 BOUT$tlat[i] =tlat
 BOUT$tlon[i] =tlon


      BOUT$dadist[i] =da$dist
   BOUT$ed2dist[i] =ed2$dist

BOUT$daaz[i]= da$az
BOUT$dabaz[i]= da$baz

BOUT$ed2az[i]= ed2$az
BOUT$ed2baz[i]=  ed2$revaz


}

print(data.frame(BOUT))




}
\keyword{misc}
