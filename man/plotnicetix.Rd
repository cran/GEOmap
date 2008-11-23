\name{plotnicetix}
\alias{plotnicetix}
\title{Plot Lat-Lon tick marks}
\description{
  Find and plot nice tick marks on projected plot
}
\usage{
plotnicetix(nex, nwhy, proj, tlen = 0.1, fonts = c("serif", "plain"), PMAT = NULL)
}
\arguments{
  \item{nex}{X coordinates}
  \item{nwhy}{Y coordinates}
  \item{proj}{prjection list}
  \item{tlen}{length for tic marks (inches)}
  \item{fonts}{Hershy font vector}
  \item{PMAT}{projection matrix from persp}
}

\value{
  Graphical Side Effects
 
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{niceLLtix, goodticdivs, getnicetix, dms}
\examples{

proj = setPROJ(7, LAT0 = 0 , LON0= -93)



rx = c(652713.4, 656017.4)
ry = c(1629271, 1631755)

plot(rx, ry, type='n', asp=1, axes=FALSE , ann=FALSE)
plotnicetix(rx, ry, proj, PMAT=NULL)

}
\keyword{misc}
