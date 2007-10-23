`addLLXY` <-
function(lats, lons, PROJ=PROJ, PMAT=NULL, col=gray(0.7), GRID=TRUE, GRIDcol=1, LABS=NULL, LABcol=1, BORDER=NULL, TICS=c(1,1))
  {

    if(missing(col)) {col=gray(0.7)}
    if(missing(GRID)) { GRID = TRUE }
    if(missing(GRIDcol)) { GRIDcol = gray(0.7) }
    if(missing(LABS)) { LABS =NULL }
    if(missing(LABcol)) { LABcol = 1 }
    
    if(missing(BORDER)) { BORDER=NULL }
    if(missing(TICS)) { TICS=NULL }


    HL = 0
    tpoints = as.list(NA)
    
########  latitudes

    if(!is.null(GRIDcol) )
      {
        col = GRIDcol
        for(i in lons)
          {
            xy = GLOB.XY(lats ,  rep(fmod(i, 360), length(lats)) ,  PROJ)
            if(!is.null(PMAT))
              {
                tem = trans3d(xy$x, xy$y, rep(0, length(xy$y)) , PMAT)
                xy$x = tem$x
                xy$y = tem$y
              }

            lines(xy$x, xy$y, col=GRIDcol, lty=2, lwd=.4, xpd=TRUE )
            HL = HL +1
            tpoints[[HL]] = xy
                    
          }

########  longitudes
        for(i in lats)
          {
            xy = GLOB.XY(rep(i, length(lons)), lons, PROJ  )

            if(!is.null(PMAT))
              {
                tem = trans3d(xy$x, xy$y, rep(0, length(xy$y)) , PMAT)
                xy$x = tem$x
                xy$y = tem$y
              }

            lines(xy$x, xy$y,  col=GRIDcol, lty=2, lwd=.4, xpd=TRUE )
            HL = HL +1
            tpoints[[HL]] = xy

          }
      }

    if(!is.null(TICS))
      {
        addTIX(lats, lons, PMAT=PMAT, col=GRIDcol, TICS=TICS, PROJ=PROJ)
        
      }
    
###  anotations
###  longitudes

    if(!is.null(LABS))
      {
        col = LABcol
        i = lats[1]
        j = lons[-1]
        
        xy = GLOB.XY(rep(i, length(j)), j , PROJ )
        
        if(!is.null(PMAT))
          {
            tem = trans3d(xy$x, xy$y, rep(0, length(xy$y)) , PMAT)
            xy$x = tem$x
            xy$y = tem$y
          }
        
###i = lats[2]
###xy2 = GLOB.XY(rep(i, length(lons)), lons  ) 
###angs = 180*atan2(xy$y-xy2$y,xy$x-xy2$x   )/pi
        
      
        signew = sign(j)
        degs = floor(abs(j))
        mins = round(60*(abs(j)-degs))
        degs = signew*degs
        

        

        for(k in 1:length(j))
          {
            #print(paste(sep=' ', degs[k], mins[k]))

            
            if(mins[k]==0)
              {
                text(xy$x[k], xy$y[k], labels=bquote(.(degs[k])*degree)  , xpd=TRUE, pos=1, col=col)
                
                
              }
            else
              {
                text(xy$x[k], xy$y[k], labels=bquote(.(degs[k])*degree ~ .(mins[k])*minute)  , xpd=TRUE, pos=1, col=col)

              }
          }
        
        i = lats[-1]
        j = lons[1]
        
        xy = GLOB.XY(i,  rep(j, length(i)), PROJ )
        if(!is.null(PMAT))
          {
            tem = trans3d(xy$x, xy$y, rep(0, length(xy$y)) , PMAT)
            xy$x = tem$x
            xy$y = tem$y
          }
        
        degs = floor(i)
        mins = round(60*(i-degs))
        for(k in 1:length(i))
          {
            if(mins[k]==0)
              {
               text(xy$x[k], xy$y[k], labels=bquote(.(degs[k])*degree)  , xpd=TRUE, pos=2, col=col)
              }
            else
              {
                 text(xy$x[k], xy$y[k], labels=bquote(.(degs[k])*degree ~ .(mins[k])*minute)  , xpd=TRUE, pos=2, col=col)
                
              }
          }
        
      }

    invisible(tpoints)
    
  }

