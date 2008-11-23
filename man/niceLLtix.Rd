\name{niceLLtix}
\alias{niceLLtix}
\title{Nice DMS coordinates}
\description{Determine a nice set of coordinates in DMS
}
\usage{
niceLLtix(rcoords)
}
\arguments{
  \item{rcoords}{vector of decimal degrees, the range will be used }
}

\value{
  \item{DD}{decimal degrees}
  \item{deg}{degrees}
  \item{min}{minutes}
  \item{sec}{seconds}
  \item{si}{sign of degrees}

  
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{dms}
\examples{

niceLLtix(c(12.5, 12.58) )
niceLLtix(c(12.57, 12.58) )

niceLLtix(c(91.5, 92.8) )
niceLLtix(c(-91.5, -92.8) )

niceLLtix(c(91.5, 93.8) )


niceLLtix(c(91.5, 95.8) )

niceLLtix(c(-91.5, -95.8) )


}
\keyword{misc}
