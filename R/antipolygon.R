`antipolygon` <-
  function(x,y,col=0, corner=1, pct=.4)
{
  if(missing(corner)) { corner=1 }

  if(missing(pct)) { pct=.4 }


  if(pct>1) { pct = pct/100 }

  
  
  ##  use the polygon in x,y to blank out (mask) the image on the screen
  ## useful for plotting contour plots and images
  ##  antipolygon(POL$x, POL$y, col=rgb(1,1,1) )
  ##  see contPfile  for an example of usage:
###   corners: 1 = LowerLeft(default) ; 2:UpperLeft 3 = UpperRight; 4=LowerRight


if( identical(tolower(corner), "lowerleft") | identical(tolower(corner), "bottomleft"))
  {corner=1 }
if( identical(tolower(corner), "upperleft") | identical(tolower(corner), "topleft"))
  {corner=2 }
if( identical(tolower(corner), "upperright") | identical(tolower(corner), "topright"))
  {corner=3 }
if( identical(tolower(corner), "lowerright") | identical(tolower(corner), "bottomright"))
  {corner=4 }
  
  u <- par("usr")

  dxu = pct*(u[2]-u[1])
  dyu = pct*(u[4]-u[3])

####  first check if polygon is 'positive (counter-clock-wise)

  n = length(x)
T.sum = sum( (x[2:(n)]-x[1:(n-1)])*(y[2:(n)]+y[1:(n-1)]) )
  if(T.sum>0) {
####  need to reverse
      x = rev(x)
      y = rev(y)
  }


  
  if(corner==1)
  {
      w1 = which.min(y)
      x=c(x[w1:n], x[1:(w1-1) ])
      y=c(y[w1:n], y[1:(w1-1) ])

      x <- c(x,x[1],u[1]-dxu,u[1]-dxu,u[2]+dxu,u[2]+dxu,u[1]-dxu)
      
      y <- c(y,y[1],u[3]-dyu,u[4]+dyu,u[4]+dyu,u[3]-dyu,u[3]-dyu)

    }
  if(corner==2)
    {

        w1 = which.min(x)
      x=c(x[w1:n], x[1:(w1-1) ])
      y=c(y[w1:n], y[1:(w1-1) ])
  
      x <- c(x,x[1],u[1]-dxu,u[2]+dxu,u[2]+dxu,u[1]-dxu, u[1]-dxu)
      
      y <- c(y,y[1],u[4]+dyu,u[4]+dyu,u[3]-dyu,u[3]-dyu,u[4]-dyu)

    }
  if(corner==3)
  {
      w1 = which.max(x)
      x=c(x[w1:n], x[1:(w1-1) ])
      y=c(y[w1:n], y[1:(w1-1) ])

              
      x <- c(x,x[1],u[2]+dxu,u[2]+dxu,u[1]-dxu, u[1]-dxu,u[1]-dxu)
      y <- c(y,y[1],u[4]+dyu,u[3]-dyu,u[3]-dyu,u[3]-dyu,u[4]+dyu)

  }

  
  if(corner==4)
  {
       w1 = which.min(y)
      x=c(x[w1:n], x[1:(w1-1) ])
      y=c(y[w1:n], y[1:(w1-1) ])

      x <- c(x,x[1], u[2]+dxu, u[1]-dxu, u[1]-dxu, u[2]+dxu, u[2]+dxu)
      y <- c(y,y[1], u[3]-dyu, u[3]-dyu, u[4]+dyu, u[4]+dyu, u[3]+dyu)

    }

  
  polygon(x,y,border=col,col=col, xpd=TRUE)

  invisible( list(x=x,y=y) )
  ## box()
}

