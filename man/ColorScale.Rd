\name{ColorScale}
\alias{ColorScale}
\title{Color Scale}
\description{
 Graded Color Scale position by locator
}
\usage{
ColorScale(z, loc = list(x = 0, y = 0),  thick=1, len=1, offset=.2, col
= rainbow(100),border='black', gradcol='black',numbcol='black',  unitscol='black', 
units = "", SIDE = 1, font = 1, fontindex =1, cex=1)
}
\arguments{
  \item{z}{values to be scaled}
  \item{loc}{x-y location boundary of plotting area, user coordinates}
  \item{thick}{width of scale bar in inches}
  \item{len}{length of scale bar in inches}
    \item{offset}{ offset from border, in inches}
  \item{col}{color palette}
    \item{border}{color for border of scale, NA=do not plot}
  \item{gradcol}{color for gradiation marks  of scale, NA=do not plot}
  \item{numbcol}{color for number values of scale, NA=do not plot}
  \item{unitscol}{color for units character string, NA=do not plot}
  \item{units}{character, units for values}
\item{SIDE}{side, 1,2,3,4 as in axis}
  \item{font}{vfont number}
  \item{fontindex}{font index number}
  \item{cex}{character expansion, see par for details}

  
}

\value{
  list 
  Graphical Side effects
  and list of bounding box for color scale:
  \item{x}{x coordinates of box}
  \item{y}{y coordinates of box}
  
}
\details{
  Locations (loc) are given in User coordinates.
  The scale is plotted relative to the
  location provided in user coordinates and offset
  by so many inches outside that unit.
  to get a scale plotted on the interior of
  a plot, send ColorScale a rectangular box
  inside the plotting region and give it a 0 offset.
  All other measures
  are given in inches.
  To suppress the plotting of a particular item,
  indicate NA for its color.

  Since the list of the bounding box is returned,
  this can be used to modify the text, e.g. change the
  way the units are displayed.


}



\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{HOZscale}
\examples{

data(volcano)

d = dim(volcano)
x=seq(from=1,by=1, length=d[1]+1)
y=seq(from=1,by=1, length=d[2]+1)
plot(range(x), range(y), type='n',  asp=1, ann=FALSE, axes=FALSE)

image(x=x, y=y, z=volcano, col = rainbow(100), add=TRUE)

z=volcano




ColorScale(volcano, loc=list(x=range(x), y=range(y)) ,
     col = rainbow(100), units = "Elev:m", font = 1, SIDE = 1)

ColorScale(volcano, loc=list(x=range(x), y=range(y)) ,
     col = rainbow(100), units = "Elev:m", font = 1, SIDE = 2)

ColorScale(volcano, loc=list(x=range(x), y=range(y)) ,
     col = rainbow(100), units = "Elev:m", font = 1, SIDE = 3)

ColorScale(volcano, loc=list(x=range(x), y=range(y)) ,
     col = rainbow(100), units = "Elev:m", font = 1, SIDE = 4)


plot(range(x), range(y), type='n',  asp=1, ann=FALSE, axes=FALSE)

##   image(x=x, y=y, z=volcano, col = rainbow(100), add=TRUE)

XAX = pretty(x)
XAX = XAX[XAX>=min(x)  & XAX<=max(x)]

axis(1, at=XAX, pos=y[1])

YAX = pretty(y)
YAX = YAX[YAX>=min(y)  & YAX<=max(y)]

axis(2, at=YAX, pos=x[1])

rect(x[1], y[1], max(x), max(y))



ColorScale(volcano, loc=list(x=range(x),  y=range(y)) ,offset=.8,
     col = rainbow(100), units = "Elev:m", font = 2, SIDE = 1)

ColorScale(volcano, loc=list(x=range(x), y=range(y)), offset=.8 ,
     col = rainbow(100), units = "Elev:m", font = 1, fontindex = 2,SIDE = 2)

ColorScale(volcano, loc=list(x=range(x), y=range(y)), offset=.2 ,
     col = rainbow(100), units = "Elev:m", font = 1, fontindex = 3, SIDE = 3)

ColorScale(volcano, loc=list(x=range(x), y=range(y)), offset=.2 ,
     col = rainbow(100), units = "Elev:m", font = 2, fontindex = 3, SIDE = 4)


plot(range(x), range(y), type='n',  asp=1, ann=FALSE, axes=FALSE)

##   image(x=x, y=y, z=volcano, col = rainbow(100), add=TRUE)

XAX = pretty(x)
XAX = XAX[XAX>=min(x)  & XAX<=max(x)]

axis(1, at=XAX, pos=y[1])

YAX = pretty(y)
YAX = YAX[YAX>=min(y)  & YAX<=max(y)]

axis(2, at=YAX, pos=x[1])

rect(x[1], y[1], max(x), max(y))


ColorScale(volcano, loc=list(x=range(x),  y=range(y)) , offset=.8,  gradcol= NA, 
     col = rainbow(100), units = "Elev:m", font = 2, SIDE = 1)

ColorScale(volcano, loc=list(x=range(x), y=range(y)), offset=.8 ,numbcol
= NA, 
     col = rainbow(100), units = "Elev:m", font = 1, fontindex = 2,SIDE = 2)

ColorScale(volcano, loc=list(x=range(x), y=range(y)), offset=.2
,unitscol  = NA,
     col = rainbow(100), units = "Elev:m", font = 1, fontindex = 3, SIDE = 3)

ColorScale(volcano, loc=list(x=range(x), y=range(y)), offset=.2 ,border
= NA, gradcol  = 'black', numbcol  = 'blue', unitscol  = 'purple', 
     col = rainbow(100), units = "Elev:m", font = 2, fontindex = 3, SIDE
= 4)


###########################

plot(range(x), range(y), type='n',  asp=1, ann=FALSE, axes=FALSE)

##   image(x=x, y=y, z=volcano, col = rainbow(100), add=TRUE)

XAX = pretty(x)
XAX = XAX[XAX>=min(x)  & XAX<=max(x)]

axis(1, at=XAX, pos=y[1])

YAX = pretty(y)
YAX = YAX[YAX>=min(y)  & YAX<=max(y)]

axis(2, at=YAX, pos=x[1])

rect(x[1], y[1], max(x), max(y))

B = ColorScale(volcano, loc=list(x=range(x), y=range(y)), offset=.2 ,border
= NA, gradcol  = NA, numbcol  = NA, unitscol  = NA, 
     col = rainbow(100), units = "Elev:m", font = 2, fontindex = 3, SIDE = 3)

text(mean(B$x), B$y[2], "scaled data", pos=3, xpd=TRUE)

text(B$x[1], mean(B$y), min(volcano), pos=2,  xpd=TRUE)
text(B$x[2], mean(B$y), max(volcano), pos=4,  xpd=TRUE)



###########################   dark background
par(fg="white")
par(bg="black")
par(col.axis="white",   col.lab="white",  col.main="white",  col.sub="white") 

plot(range(x), range(y), type='n',  asp=1, ann=FALSE, axes=FALSE,
fg='white' )
image(x=x, y=y, z=volcano, col = rainbow(100), add=TRUE)

XAX = pretty(x)
XAX = XAX[XAX>=min(x)  & XAX<=max(x)]

axis(1, at=XAX, pos=y[1])

YAX = pretty(y)
YAX = YAX[YAX>=min(y)  & YAX<=max(y)]

axis(2, at=YAX, pos=x[1])

rect(x[1], y[1], max(x), max(y), border='white')


ColorScale(volcano, loc=list(x=range(x), y=range(y)) ,offset=.6, 
gradcol= 'black',  unitscol  =rgb(.9, .9, 1) ,  numbcol  =rgb(.9, 1, .9) , border="white",
     col = rainbow(100), units = "Elev:m", font = 2, fontindex = 3, SIDE = 1)

ColorScale(volcano, loc=list(x=range(x), y=range(y)), offset=.8
,numbcol= rgb(1, .85, .85) , 
     col = rainbow(100), units = "Elev:m", font = 1, fontindex = 2,SIDE = 2)

ColorScale(volcano, loc=list(x=range(x), y=range(y)), offset=.2,unitscol  = NA,
     col = rainbow(100), units = "Elev:m", font = 1, fontindex = 3, SIDE = 3)

ColorScale(volcano, loc=list(x=range(x), y=range(y)), offset=.2 ,border
= NA, gradcol  = 'white', numbcol  = 'blue', unitscol  = 'purple', 
     col = rainbow(100), units = "Elev:m", font = 2, fontindex = 3, SIDE = 4)





plot(range(x), range(y), type='n',  asp=1, ann=FALSE, axes=FALSE,
fg='white' )

XAX = pretty(x)
XAX = XAX[XAX>=min(x)  & XAX<=max(x)]

axis(1, at=XAX, pos=y[1])

YAX = pretty(y)
YAX = YAX[YAX>=min(y)  & YAX<=max(y)]

axis(2, at=YAX, pos=x[1])

rect(x[1], y[1], max(x), max(y), border='black')



ColorScale(volcano, loc=list(x=c(20, 40), y=c(10, 40)), thick=.2, offset=0 ,  
     col = rainbow(100), units = "Elev:m", font = 1, fontindex = 2,SIDE
= 2, cex=.5)








}
\keyword{aplot}
