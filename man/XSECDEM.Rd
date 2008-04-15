\name{XSECDEM}
\alias{XSECDEM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Cross Sections Using RPMG }
\description{
  This function Takes a Digital Elevation Map (or any surface)
  and illustrates how to take interactive cross sections
  with RPMG through the surface.
}
\usage{
XSECDEM(Data, labs, demo=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{Structure with x, y, z components, typical of
  contoured surfaces or digital images}
\item{labs}{Vector of labels for Buttons used in the RPMG }
\item{demo}{Argument used to turn off interactive part.  Default is
  FALSE, but for package construction is set to TRUE so no interaction
  is required.}

}
\details{
  XSECDEM is an example stub illustrating the use of RPMG.
  The idea is to set up a while() loop that uses input from the
  locator() function to execute or analyze data depending on user
  defined buttons.  Actions are executed when the button clicked
  matches the list of names provided by the user.
 
}
\value{
  No return values
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}
\note{
  This code is designed as an example of how to set up a Really Poor
  Man's GUI.  The demo argument is supplied so that this code will run
  without user input, as when creating a checks for package
  construction.



  
}
\seealso{whichbutt, rowBUTTONS}
\examples{
data(volcano)
attr(volcano, 'dx') =10
attr(volcano, 'dy') =10
mybutts = c("DONE", "REFRESH", "rainbow", "topo", "terrain", "CONT",
"XSEC","PS" )
###  in the following change demo=FALSE to get interactive behavior
XSECDEM(volcano, mybutts, demo=TRUE)



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{iplot}
