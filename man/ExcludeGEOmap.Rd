\name{ExcludeGEOmap}
\alias{ExcludeGEOmap}
\title{Exclude GEOmap Strokes}
\description{
  Select sections of a MAP-list structure based on stroke index
}
\usage{
ExcludeGEOmap(MAP, SEL, INOUT = "out")
}
\arguments{
  \item{MAP}{Map List}
  \item{SEL}{Selection of stroke indeces to include or exclude}
  \item{INOUT}{text, "in" means include, "out" means exclude}
}

\value{
  \item{MAP}{list}
  
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{getGEOmap, plotGEOmap, SELGEOmap, boundGEOmap }
\examples{
data(coastmap)

###  extract (include)  the first 6 strokes from world map


A1 = ExcludeGEOmap(coastmap, 1:6, INOUT="in")
print(A1$STROKES$nam)


}
\keyword{misc}
