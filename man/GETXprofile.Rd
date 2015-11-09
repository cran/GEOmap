\name{GETXprofile}
\alias{GETXprofile}

\title{Cross sectional profile through a digital elevation map }
\description{
  Example of how to use RPMG button functions.  This example
  shows how to plot a DEM and interactively change the plot
  and find projected cross-sections through a surface.
}
\usage{
GETXprofile(jx, jy, jz, LAB = "A", myloc = NULL, PLOT = FALSE, NEWDEV=TRUE,  asp=1)
}

\arguments{
  \item{jx, jy}{locations of grid lines at which the values in 'jz' are measured.}
  \item{jz}{a matrix containing the values to be plotted   }
  \item{LAB}{Alphanumeric (A-Z) for labeling a cross section }
  \item{myloc}{Out put of Locator function }
  \item{PLOT}{logical. Plot is created if TRUE }
  \item{NEWDEV}{logical. Plot is on a new device if TRUE }
  \item{asp}{aspect ration for plotting, see par }
  
}
\details{
  The program uses a similar input format as image or contour,
  with structure from the locator() function of x and y coordinates
  that determine where the cross section is to be extracted.
 
}
\value{
  Returns a list of x,z values representing the projected values along
  the cross section.
  \item{RX}{distance along cross section}
  \item{RZ}{values extracted from the elevation map}
}
\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
  The program is an auxiliary program provided to illustrate the RPMG
  interactive R analysis.
}
\seealso{locator, image}
\examples{

\dontrun{ 
#######  get data 
   data(volcano)
####  extract dimensions of image
   nx = dim(volcano)[1]
   ny = dim(volcano)[2]

###  establish units of image
   jx = 10*seq(from=0, to=nx-1)
   jy = 10*seq(from=0, to=ny-1)

####  set a letter for the cross section
   LAB = LETTERS[1]

###  coordinates of cross section on image
###  this is normally set by using the locator() function
   x1 = 76.47351
   y1 = 231.89055
   x2 = 739.99746
   y2 = 464.08185

## extract and plot cross section

 GETXprofile(jx, jy, volcano, myloc=list(x=c(x1, x2), y=c(y1, y2)), LAB=LAB, PLOT=TRUE)
            }
}

\keyword{iplot}
