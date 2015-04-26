\name{XSECEQ}
\alias{XSECEQ}
\title{Iinteractive earthquake cross section}
\description{
 Iinteractive earthquake cross section
}
\usage{
XSECEQ(MAP, EQ, XSECS = NULL, labs = c("DONE", "REFRESH", "XSEC",
"MSEC"),
 width = 10, kmaxes = TRUE, pch = ".", demo = FALSE)
}

\arguments{
  \item{MAP}{Geologic Map Structure}
  \item{EQ}{list of earthquakes}
  \item{XSECS}{list of cross sections}
  \item{labs}{labels for cross sections}
  \item{width}{width of swaths}
  \item{kmaxes}{logical, TRUE=keep all cross sections same depth}
  \item{pch}{plotting character}
  \item{demo}{Logical, TRUE=not-interactive}
}

\value{
  Graphical side effects and creates cross-sectional swaths returned as
  a list, see eqswath for list structure.
  
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{XSECDEM, eqswath, XSECwin}
\examples{

\dontrun{

##########  get map of Japan
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
data(EHB.LLZ)

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

labs=c("DONE","REFRESH", "XSEC", "MSEC",  "KMAXES", "CONT", "width", "PS" )
    
NSWath = XSECEQ(  MAP, EQ , labs=labs, width=30, demo=FALSE  )


data(NSWath)
NSWath2 = XSECEQ(  MAP, EQ ,XSECS=NSWath, labs, width=30, demo=TRUE  )   

}


}

\keyword{hplot}
\keyword{iplot}
