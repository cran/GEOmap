\name{STROKEinfo}
\alias{STROKEinfo}
\title{Stroke Information
}
\description{print stroke information from a GEOmap data base
}
\usage{
STROKEinfo(map, w = 1, h = NULL)
}
\arguments{
  \item{map}{GEOmap data list
}
\item{w}{which strokes to extract, vector of number indices or single
  string
  to match names in data base list
}
\item{h}{numeric vector of columns of data base, or vector of characters
  to match names.

  
}
}
\details{Uses grep to match names so can have short names
}
\value{data.frame of extracted strokes
}
\note{Use gsub to change the names of strokes.
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{gsub
}
\examples{

data(coastmap)
STROKEinfo(coastmap, h="nam", w="Indo")

STROKEinfo(coastmap, w="Indo", h=c("nam", "col" ) )


}
\keyword{misc}
