\name{plothypos}
\alias{plothypos}
\title{Plot Edicenters
}
\description{Plot hypocenter  color coded to depth
  and size scaled by magnitude.
}
\usage{
plothypos(lat, lon, z, proj, mag = NULL, cex = 0.4, pch =21, PMAT = NULL, alpha = NULL)
}
\arguments{
  \item{lat}{Latitude
}
  \item{lon}{Longitude
}
  \item{z}{km Depth, (positive down)
}
  \item{proj}{Projection structure
}
  \item{mag}{Magnitude
}
  \item{cex}{character expansion
}
\item{pch}{plotting character, default=21}

  \item{PMAT}{transformation matrix
}
  \item{alpha}{transparency factor
}
}
\details{
  Adds hypocenters to an existing
  plot.
}
\value{Graphical Side effects.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
  The events are color coded according to depth.
  
  Only a few devices can handle
  transparency effects.
}


\seealso{
  plotGEOmapXY, XSECEQ, eqswath, getmagsize
  
}
\examples{

data(EHB.LLZ)
data(japmap)

RLAT = range(japmap$POINTS$lat)
RLON = range(japmap$POINTS$lon)

JLAT = expandbound(RLAT, .1) 
JLON = expandbound(RLON, .1) 

PROJ = japmap$PROJ
##############  select the events in the region
sel = which(
EHB.LLZ$lat > JLAT[1] &
EHB.LLZ$lat < JLAT[2] &
EHB.LLZ$lon  > JLON[1] &
EHB.LLZ$lon < JLON[2])

plotGEOmapXY(japmap , PROJ=PROJ,   add=FALSE, MAPcol="black") 

plothypos(EHB.LLZ$lat[sel], EHB.LLZ$lon[sel], EHB.LLZ$z[sel], PROJ,
mag=NULL,  cex=.8)


\dontrun{

fn = "/home/lees/WORK/SENDAI.EVENT/catsearch.8757"

g = getANSS(fn, skip=2)
g$jd = getjul(g$yr, g$mo, g$dom)

sel = which(
g$lat > JLAT[1] &
g$lat < JLAT[2] &
g$lon  > JLON[1] &
g$lon < JLON[2])

olat = g$lat[sel]
olon = g$lon[sel]
ordz = g$z[sel]

mag = g$mag[sel]

gm = getmagsize(mag)


plotGEOmapXY(japmap , PROJ=PROJ,   add=FALSE, MAPcol="black") 
plothypos(g$lat[sel], g$lon[sel], g$z[sel], PROJ,
mag=NULL,  cex=gm)


plotGEOmapXY(japmap , PROJ=PROJ,   add=FALSE, MAPcol="black") 
plothypos(olat, olon, ordz, PROJ,
mag=NULL,  cex=gm)


plotGEOmapXY(japmap , PROJ=PROJ,   add=FALSE, MAPcol="black") 
plothypos(olat, olon, ordz, PROJ,
mag=mag,  cex=1 )



##################  transparent plot
pdfname = local.file('TOHOKU', "pdf")

cairo_pdf(file = pdfname , width = 8, height = 10)
plotGEOmapXY(japmap , PROJ=PROJ,   add=FALSE, MAPcol="black") 
plothypos(olat, olon, ordz, PROJ,
mag=mag,  cex=1, alpha=.3 )
dev.off()
################## 



}




}
\keyword{misc}
