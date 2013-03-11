\name{rose}
\alias{rose}
\title{Rose Diagram
}
\description{Rose diagram of angle orientations or directions
}
\usage{
rose(angles, bins, x = 0, y = 0, col = "black", border = "black",
annot = FALSE, main = "", prop = 1, pts = FALSE, cex = 1, pch = 16,
dotsep = 40, siz = 1, LABS = LABS, LABangle = 180, add = FALSE, SYM = FALSE)
}
\arguments{
  \item{angles}{numeric, vector of angles in radians
}
  \item{bins}{integer, number of bins
}
  \item{x}{numeric, x location on page
}
  \item{y}{numeric, y location on page
}
  \item{col}{color for pie slices
}
  \item{border}{color for pie borders
}
  \item{annot}{logical, annotation
}
  \item{main}{character, main title
}
  \item{prop}{proportional plotting, default  = 1
}
  \item{pts}{logical, add points (default=FALSE)
}
  \item{cex}{character expansion
}
  \item{pch}{plotting character
}
  \item{dotsep}{separation of dots
}
  \item{siz}{size of plot
}
  \item{LABS}{Labels
}
  \item{LABangle}{angle for plotting Label angles
}
  \item{add}{logical, add to plot (default=FALSE)
}
  \item{SYM}{logical, symmetric rose diagram (FALSE)
}
}
\details{
  Create a rose diagram or add rose diagram to an existing plot.
  Used for plotting geographic orientations or directions.
}
\value{list:
  \item{usector}{sector angles}
  \item{uradius}{sector radii}
  \item{usizx}{x size scale}
  \item{usizy}{y size scale}
  \item{x}{x center on page}
  \item{y}{y center on page}
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
  For symmetric plots, bins are rotated and added together,
  then the reflection is made.
}


\seealso{package RFOC for distributions on a sphere
}
\examples{

ff=c(23,27,53,58,64,83,85,88,93,99,100,
  105,113,113,114,117,121,123,125,126,
  126,126,127,127,128,128,129,132,132,
  132,134,135,137,144,145,145,146,153,
  155,155,155,157,163,165,171,172,179,181,186,190,212)




rose((ff-90)*pi/180, 50, x=0, y=0, LABS = c("N", "S", "W", "E"), 
annot=TRUE,border='white',LABangle=135, siz =sqrt(2), SYM=FALSE)

rose((ff-90)*pi/180, 50, x=0, y=0, LABS = c("N", "S", "W", "E"), 
annot=TRUE,border='white',LABangle=135, siz =sqrt(2), SYM=TRUE)





}
\keyword{misc}
