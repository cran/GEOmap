`zebra` <-
function(x,y,Dx, dx, dy, lab="", units="", col=c("black", "white"), cex=1, xpd=TRUE,  PMAT=NULL)
{
###  zebra: draw a zebra bar km scale on a map; Km ; KM scale; kmscale
###  x, y = location of left point 
###   DX  =length km
###    dx  = alternating distance
###   dy = thickness of zebra


  if(missing(PMAT)) { PMAT=NULL  }
  if(missing(lab)) { lab=NULL  }
  if(missing(col)) { col=c("black", "white")  }
  if(missing(cex)) { cex=1  }
  if(missing(units)) { units=NULL }
  if(missing(xpd)) { xpd=TRUE }

  
  for(i in 1:(Dx/dx))
    {
      px1 = x+(i-1)*dx
      py1 = y
      px2 = x+(i)*dx
      py2 = y+dy

      if(!is.null(PMAT))
        {
          tem1 = trans3d(px1, py1, rep(0, length(py1)) , PMAT)
          tem2 = trans3d(px2, py2, rep(0, length(py2)) , PMAT)
          rect(tem1$x[1], tem1$y[1], tem2$x[1], tem2$y[1], col=col[i%%2 + 1] , xpd=xpd)

        }
      else
        {
          rect(px1, py1, px2, py2, col=col[i%%2 + 1], xpd=xpd)
        }
      
    }


  if(!is.null(PMAT))
    {
      tem1 = trans3d(x, y, rep(0, length(y)) , PMAT)
      text(tem1$x[1], tem1$y[1], labels="0", pos=1, cex=cex, xpd=xpd)
      
      tem1 = trans3d(px2, y, rep(0, length(y)) , PMAT)
      text(tem1$x[1], tem1$y[1], labels=Dx, pos=1, cex=cex, xpd=xpd )
      
      tem1 = trans3d((x+px2)/2, y, rep(0, length(y)) , PMAT)
      text(tem1$x[1], tem1$y[1], labels=lab, pos=1, cex=cex , xpd=xpd)
    }
  else
    {
      text(x,y, labels="0", pos=1, cex=cex, xpd=xpd)
      text(px2, y, labels=Dx, pos=1, cex=cex, xpd=xpd )
      text((x+px2)/2, y, labels=lab, pos=1, cex=cex, xpd=xpd)
    }

}

