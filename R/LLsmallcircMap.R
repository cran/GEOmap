`LLsmallcircMap` <-
    function(phicen, lamcen, worldmap,  eqlat, eqlon, MAXR=100,
             circol=rgb(1.0, .8, .8),
             mapcol=rgb(0, 0, 0),
             eqcol=rgb(.8, .8, 1), pch=25, ecex=1)
  {
      if(missing(eqcol)) { eqcol=rgb(.8, .8, 1) }
      
      if(missing(mapcol)) { mapcol=rgb(0, 0, 0) }
      if(missing(circol)) { circol=rgb(1.0, .8, .8)  }


      if(length(eqcol)==2)
      {
          kcol1 = eqcol[1]
          kcol2 = eqcol[2]
      }
      else
      {
          kcol1 = eqcol[1]
          kcol2 = 'black'
      }




      
      
    phi1 =phicen
    lam0= lamcen
    mina = 20 
    maxa = MAXR+10
    DEGRAD = 0.017453293
    
    phi1 = DEGRAD * phi1
    lam0 = DEGRAD * lam0
    
    j = MAXR
    lat = DEGRAD * (phicen-j)
    lon = lam0
    xy = lamaz.eqarea(phi1,  lam0,lat, lon)
    r = sqrt(xy$x^2 + xy$y^2)
      plotRAD = r
      C= circle()
      C$x = c(C$x, C$x[1])
      C$y = c(C$y, C$y[1])


plot(c(-r,r), c(-r,r), asp=TRUE, type='n', axes=FALSE, xlab='', ylab='', xaxs='i',yaxs='i',)

    points(0,0, pch=3, col='black')
    
    
##############  plot the circles

   Jcircs =  seq(from=mina, to=maxa, by=20)
##############  plot the circles
    if(!is.na(circol) )
    {
        for(j in Jcircs )
        {
            
            lat = DEGRAD * (phicen-j)
            lon = lam0
            xy = GEOmap::lamaz.eqarea(phi1,  lam0,lat, lon)
            r = sqrt(xy$x^2 + xy$y^2)
            ## points(xy$x, xy$y, col=2)
            text(xy$x, xy$y, labels=j, pos=3)
            
            lines(r*C$x, r*C$y, col=circol , lwd=2)
            
            ## grid.circle(0, 0, r)

            
        }
    }
    else
      {
          j = max(Jcircs)
          lat = DEGRAD * (phicen-j)
            lon = lam0
            xy = GEOmap::lamaz.eqarea(phi1,  lam0,lat, lon)
            r = sqrt(xy$x^2 + xy$y^2)
            ## points(xy$x, xy$y, col=2)
          #  text(xy$x, xy$y, labels=j, pos=3)
            
            lines(r*C$x, r*C$y, col='black' , lwd=2)
            


      }

   
    
###############   plot the map
    for(i in 1:length(worldmap$STROKES$index))
      {
        
### 
        j1 = worldmap$STROKES$index[i]+1
        j2 = j1+worldmap$STROKES$num[i]-1
        
        lat = DEGRAD*worldmap$POINTS$lat[j1:j2]
        lon = DEGRAD*worldmap$POINTS$lon[j1:j2]
        
        
        xy = lamaz.eqarea(phi1,  lam0,lat, lon)
        
        are = sqrt(xy$x^2 + xy$y^2)
        
        flag = are>plotRAD
        xy$x[flag] = NA
        xy$y[flag] = NA
        
     
###  maybe figure this out later:
#### polygon(xy$x, xy$y, col=mapcol)
          lines(xy$x, xy$y, col=mapcol)
        
      }

#############  plot the earthquakes

    DOEQS = !is.null(eqlat) & !is.null(eqlon)
     DOEQS =   DOEQS &  length(eqlat) > 0 & length(eqlon)
if(DOEQS==TRUE)
  {
    Exy = lamaz.eqarea(phi1,  lam0, DEGRAD *eqlat, DEGRAD *eqlon)
    
    are = sqrt(Exy$x^2 + Exy$y^2)
    
    flag = are>plotRAD
    Exy$x[flag] = NA
    Exy$y[flag] = NA

    points(Exy$x, Exy$y, pch=pch, col=kcol1, bg=kcol2, cex=ecex  )
    ## points(xy$x, xy$y, pch=21, col="black")
    
  }
    
    GIVEBACK =list(r=plotRAD,  Exy=Exy)
    return(GIVEBACK)
  }

