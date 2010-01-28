\name{LLlabel}
\alias{LLlabel}
\title{Nice Lat-Lon Label
}
\description{Create a text string for Lat-Lons
}
\usage{
LLlabel(DD, dir = 1, ksec = -1)
}
\arguments{
  \item{DD}{Decimal degrees
}
  \item{dir}{direction, NS or EW
}
  \item{ksec}{number of decimals for seconds
}
}
\details{creates text labels with minutes and seconds
  if needed.
  
}
\value{character string
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{niceLLtix
}
\examples{


DD = -13.12345

k = LLlabel(DD)



}
\keyword{misc}
