\name{list.GEOmap}
\alias{list.GEOmap}
\title{List stroke points in a GEOmap}
\description{List stroke points in a GEOmap
}
\usage{
list.GEOmap(MAP, SEL = 1)
}
\arguments{
  \item{MAP}{GEOmap list, with LL list }
  \item{SEL}{index, selecttion of specific strokes}
}
\details{
   Returns a GEOmap list from the output of GEOmap.list .
   This is used to repack a GEOmap list.
   Tis function can be used to create a new geomap if you have only
   strokes.  See example.  Can be used to convert
   a gmt map file (in ascii text format) to GEOmap.
   
  
}
\value{
  GEOmap list
  \item{STROKES}{Metadata for strokes}
  \item{POINTS}{list, lat=vector, lon=vector}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{GEOmap.cat, GEOmap.Extract, GEOmap.CombineStrokes, GEOmap.list}
\examples{

data(coastmap)

length(coastmap$STROKES$nam)

G = GEOmap.list(coastmap, 1)

length(G$STROKES$nam)

H = list.GEOmap(G)

length(H$STROKES$nam)

plotGEOmap(H)

##############   if you have a set of simple strokes
#####  make your own geomap:

latlon=list()
latlon$lat=c(39.8780395624,39.7488080389,39.4903449921,39.2964977069,
39.1995740643,39.1349583026,38.9088031365,38.6180322088,38.3272612810,
38.0041824724,37.8749509489,37.8749509489,38.3272612810,38.4888006853,
38.8118794939,39.0057267791,39.2318819452,39.5872686346,39.9426553241)
latlon$lon=c(136.6629878969,136.3444990720,136.0715086507,136.0715086507,
135.6165246151,135.0250453689,134.9795469653,134.9795469653,135.0705437724,
135.2525373866,135.7530198258,137.0724735289,137.3454639502,137.4364607574,
138.0734384071,138.0734384071,137.8004479858,137.7549495822,137.2544671431)

GLL=list()
GLL$lat=c(38.0552647517,38.1533772893,38.2754431875,
38.3672221979,38.5260793869,38.6483246519,38.7701056377,
38.8976069603,38.9457673342,38.9998962787,39.1025327692,
39.1927889270,39.3801557421,39.5193850467)
GLL$lon=c(135.7446171004,135.8598134616,135.9053532164,
135.9978522791,136.1369466401,136.3703056863,136.6044613488,
136.8081531656,136.9649782331,137.1064020435,137.2564343909,
137.4067379892,137.5747171917,137.6637851576)

LL =list()
LL[[1]] = latlon
LL[[2]] = GLL

J = list(LL=LL)


GL = list.GEOmap(J)

plotGEOmapXY(GL)


}
\keyword{misc}
