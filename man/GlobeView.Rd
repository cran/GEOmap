\name{GlobeView}
\alias{GlobeView}
\title{Global Plot}
\description{
  Plot global view of the earth
}
\usage{
GlobeView(phicen, lamcen, worldmap, MAXR, SEL = 1, circol = rgb(1, 0.8, 0.8), innercol = "white", linecol = rgb(0, 0, 0), mapcol = rgb(0, 0, 0), backcol = "white", add=FALSE)
}
\arguments{
  \item{phicen}{Latitude}
  \item{lamcen}{Longitude}
  \item{worldmap}{Map List}
  \item{MAXR}{Maximum radius (degrees)}
  \item{SEL}{Selection index from map}
  \item{circol}{color for concentric circles}
  \item{innercol}{inner color}
  \item{linecol}{line color, NA=do not plot}
  \item{mapcol}{map fill color, NA=do not fill polygon}
  \item{backcol}{background color}
  \item{add}{logical, FALSE means start a new plot}
  
}
\details{
  Creates a plot of view of the globe from
  a point in space using an Equal-Area projection.
  Uses the lamaz.eqarea routine for projection. (Lambert-Azimuthal Equal
  Area).  Using NA for linecol or mapcol means do not plot lines or fill
  polygons respectively.
}
\value{
  \item{Perimeter}{x,y points around the perimeter of the plot}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{plotGEOmap, lamaz.eqarea}
\examples{
data(coastmap)

phicen  =32.20122+5
lamcen  = 335.7092+20
MAXR    = 100

carolinablue = rgb(83/255, 157/255, 194/255)


SEL=which( coastmap$STROKES$code=="C")
SEL = c(SEL, which(coastmap$STROKES$nam=="GreatBritain"),
which(coastmap$STROKES$nam=="Japan"), which(coastmap$STROKES$nam=="Ireland"))


PER = GlobeView(phicen, lamcen, SEL=SEL, coastmap, MAXR, linecol=rgb(.2, .2, .2), mapcol=rgb(.8, .8, .8), 
innercol=carolinablue , circol=carolinablue ,    backcol="white")

}
\keyword{misc}
