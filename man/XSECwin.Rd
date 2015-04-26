\name{XSECwin}
\alias{XSECwin}

\title{Cross sectional plot with earthquakes projected}
\description{
  Cross section of earthquakes.
}
\usage{
XSECwin(SW, iseclab = 1, xLAB = "A",
labs = c("DONE", "REFRESH", "PS"), width = 10, demo = FALSE)
}

\arguments{
  \item{SW}{list of swath data}
  \item{iseclab}{section number}
  \item{xLAB}{Label}
  \item{labs}{labels}
  \item{width}{width of swath}
  \item{demo}{logical, TRUE=not interactive}
}
\details{
  Called by XSECEQ; but this can be run independantly if
  plots are needed after interactive processing.
}
\value{
  Graphical Side effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{eqswath, XSECEQ}
\examples{

\dontrun{ 
library(geomapdata)

 data('japmap', package='geomapdata' )
proj = setPROJ(type = 2, LAT0=35.358,LON0=138.731)

NIHON = list(lat=range(c(japmap$STROKE$LAT1, japmap$STROKE$LAT2)) , 
             lon = range(c(japmap$STROKE$LON1, japmap$STROKE$LON2)))


xyjap = GLOB.XY(NIHON$lat, NIHON$lon, proj)

NIHON = c(NIHON, xyjap)
MAP = list()
MAP[[1]] = NIHON
attr(MAP, "XYLIM") <- NIHON
attr(MAP, "PROJ") <- proj

MAP[[2]] = japmap

###########  load Engdahl earthquake Data base
########  
data('EHB.LLZ' )

flagEHB = EHB.LLZ$lat>=NIHON$lat[1] &  EHB.LLZ$lat<=NIHON$lat[2] &
RPMG::fmod(EHB.LLZ$lon, 360)>+NIHON$lon[1] &  RPMG::fmod(EHB.LLZ$lon,
360)<=NIHON$lon[2]

eqJ =   GLOB.XY(EHB.LLZ$lat[flagEHB], EHB.LLZ$lon[flagEHB], proj)

EQ =list()
EQ[[1]]=list(lat=EHB.LLZ$lat[flagEHB], lon=EHB.LLZ$lon[flagEHB] ,
x=eqJ$x, y=eqJ$y, z=EHB.LLZ$z[flagEHB], col="brown", pch=".", cex=1.5)

rz = NULL
for(i in 1:length(EQ))
{
rz = range(c(rz, EQ[[1]]$z), na.rm=TRUE )

}

for(i in 1:length(EQ))
{
iz = RPMG::RESCALE(EQ[[i]]$z, 1, 100, rz[1], rz[2])
EQ[[i]]$COL = rainbow(100)[iz]
}

labs=c("DONE","REFRESH", "XSEC", "MSEC",  "KMAXES", "CONT", "width",
"PS" )
##  load example cross sections:
 data(NSWath)
NSWath2 = XSECEQ(  MAP, EQ ,XSECS=NSWath, labs, width=30, demo=TRUE  )

#######  show cross sections:
   for(i in 1:length(NSWath))
{

## dev.new()
LAB = attr(NSWath[[i]], "LAB")

XSECwin( NSWath[[i]] , iseclab=i, xLAB=LAB , labs=NULL, demo=TRUE  )   
}

}
}

\keyword{hplot}
\keyword{iplot}
