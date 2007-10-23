`antipolygon` <-
function(x,y,col=0)
{
  ##  use the polygon in x,y to blank out (mask) the image on the screen
  ## useful for plotting contour plots and images
  ##  antipolygon(POL$x, POL$y, col=rgb(1,1,1) )
  ##  see contPfile  for an example of usage:
  
  u <- par("usr")
  x <- c(x,x[1],u[1],u[1],u[2],u[2],u[1])
  y <- c(y,y[1],u[3],u[4],u[4],u[3],u[3])
  polygon(x,y,border=col,col=col)
  ## box()
}

