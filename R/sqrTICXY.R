sqrTICXY<-function(prsurf, proj,  side=c(1,2,3,4),  LLgrid=TRUE, col="black", colt="black", font=2, cex=1, lty=2, lwd=1, pcex=1, TICS=NULL)
  {
   
    ########  prsurf is any structure with a list of list(x=0, y=0)
    ########   proj is a projection from GEOmap
    ########   LLgrid=TRUE  put grid on map
####### col = color of lines
##########  colt = color of text
    
    if(missing(LLgrid))  LLgrid=TRUE
    if(missing(col))  col="black"
    if(missing(colt))  colt="black"
    if(missing(side))  side=c(1,2,3,4)
    if(missing(font)) font=2
    if(missing(cex))cex=1
    if(missing(lty))lty=2
    if(missing(lwd))lwd=1
    if(missing(pcex))pcex=1    
    if(missing(TICS)) TICS==NULL   
    
    
      JNUM = 100000

      botleft.x = min(prsurf$x, na.rm=TRUE)
      botleft.y = min(prsurf$y, na.rm=TRUE)

      topright.x = max(prsurf$x, na.rm=TRUE)
      topright.y = max(prsurf$y, na.rm=TRUE)
 

    rect(botleft.x, botleft.y,topright.x,  topright.y, xpd=TRUE)

    B1 = XY.GLOB(botleft.x, botleft.y, proj)
    B2 = XY.GLOB(botleft.x, topright.y, proj)
    B3 = XY.GLOB(topright.x, botleft.y, proj)
    B4 = XY.GLOB(topright.x, topright.y, proj)

############################################
      #############  left side of plot:
    if(is.null(TICS))
      {
        BLATleft = pretty(c(B1$lat, B2$lat))
      }
    else
      {
        BLATleft = TICS$lat
        
      }
    
    yleft=seq(from=botleft.y, to=topright.y, length=JNUM);
      xleft=rep(botleft.x, JNUM)
    
    beelat = XY.GLOB(xleft,yleft, proj)

    iBLATleft = BLATleft[BLATleft<max(beelat$lat) & BLATleft>min(beelat$lat)]

    Fbeesleft = findInterval(iBLATleft, beelat$lat, all.inside=TRUE)

      xy = XY.GLOB(xleft[Fbeesleft], yleft[Fbeesleft], proj)

      MYLEFT = cbind(iBLATleft,  xleft[Fbeesleft], yleft[Fbeesleft], xy$lat, xy$lon )

      if(any(side==2))
        {
          points(MYLEFT[,2], MYLEFT[,3], pch=3, xpd=TRUE, cex=pcex)

          if(!is.null(colt))
            {
              for(j in 1:length(xleft[Fbeesleft]))
                {
                  text(MYLEFT[j,2], MYLEFT[j,3], labels=bquote(.(MYLEFT[j,1])*degree) , pos=2, xpd=TRUE, font=font, cex=cex, col=colt)
                  
                }
            }
          
        }
############################################
 #############  right side of plot:

     if(is.null(TICS))
      {
      BLATright = pretty(c(B3$lat, B4$lat))
      }
    else
      {
        BLATright = TICS$lat
        
      }
    
      yright=seq(from=botleft.y, to=topright.y, length=JNUM);
      xright=rep(topright.x, JNUM)
      
      beelat = XY.GLOB(xright,yright, proj)

      iBLATright = BLATright[BLATright<max(beelat$lat) & BLATright>min(beelat$lat)]

      Fbeesright = findInterval(iBLATright, beelat$lat, all.inside=TRUE)

      xy = XY.GLOB(xright[Fbeesright], yright[Fbeesright], proj)

      MYRIGHT = cbind(iBLATright,  xright[Fbeesright], yright[Fbeesright], xy$lat, xy$lon )


      
      if(any(side==4))
        {
          points(MYRIGHT[,2], MYRIGHT[,3], pch=3, xpd=TRUE, cex=pcex)

          if(!is.null(colt))
            {
              for(j in 1:length(xright[Fbeesright]))
                {
                  text(MYRIGHT[j,2], MYRIGHT[j,3], labels=bquote(.(MYRIGHT[j,1])*degree) , pos=4, xpd=TRUE, font=font, cex=cex, col=colt)
                  
                }
            }
        }
############################################

#############  bottom side of plot:
      
    
    if(is.null(TICS))
      {
        BLONbot= pretty(c(B1$lon, B3$lon))
      }
    else
      {
        BLONbot= TICS$lon
      }


    
      xbot=seq(from=botleft.x, to=topright.x, length=JNUM);
      ybot=rep(botleft.y, JNUM)
      beelon = XY.GLOB(xbot,ybot, proj)
      iBLONbot = BLONbot[BLONbot<max(beelon$lon) & BLONbot>min(beelon$lon)]

      Fbeesbot = findInterval(iBLONbot, beelon$lon, all.inside=TRUE)
      
      xy = XY.GLOB(xbot[Fbeesbot], ybot[Fbeesbot], proj)

      MYBOT = cbind(iBLONbot,  xbot[Fbeesbot], ybot[Fbeesbot], xy$lat, xy$lon )

      if(any(side==1))
        {
          points(MYBOT[,2], MYBOT[,3], pch=3, xpd=TRUE, cex=pcex)

           if(!is.null(colt))
            {
          for(j in 1:length(MYBOT[,2]))
            {
              text(MYBOT[j,2], MYBOT[j,3], labels=bquote(.(MYBOT[j,1])*degree), pos=1, xpd=TRUE, font=font, cex=cex, col=colt)
            }
        }
        }
############################################
#############  top  side of plot:
      
       if(is.null(TICS))
      {
         BLONtop= pretty(c(B2$lon, B4$lon))
      }
    else
      {
        BLONtop= TICS$lon
      }


     

      xtop=seq(from=botleft.x, to=topright.x, length=JNUM);
      ytop=rep(topright.y, JNUM)
      beelon = XY.GLOB(xtop,ytop, proj)
      iBLONtop = BLONtop[BLONtop<max(beelon$lon) & BLONtop>min(beelon$lon)]

      Fbeestop = findInterval(iBLONtop, beelon$lon, all.inside=TRUE)
      xy = XY.GLOB(xtop[Fbeestop], ytop[Fbeestop], proj)

      MYTOP = cbind(iBLONtop,  xtop[Fbeestop], ytop[Fbeestop], xy$lat, xy$lon )


      if(any(side==3))
        {
          
          points(MYTOP[,2], MYTOP[,3], pch=3, xpd=TRUE, cex=pcex)

           if(!is.null(colt))
            {
          for(j in 1:length(MYTOP[,2]))
            {
              text(MYTOP[j,2], MYTOP[j,3], labels=bquote(.(MYTOP[j,1])*degree), pos=3, xpd=TRUE, font=font, cex=cex, col=colt)
            }
        }
        }



     ############################################ 
    if(LLgrid==TRUE)
      {


      ###  m1 = match( iBLONtop ,  iBLONbot)
       ### thelons = iBLONbot[m1[!is.na(m1)]]
        thelons =  unique(c(MYTOP[,1], MYBOT[,1]))
        
        ############################################
        for(i in 1:length(thelons))
          {
            j = which(thelons[i]==MYBOT[,1] )
            if(length(j)<1)
              {
                LL1 = list(lat=min(MYBOT[,4], na.rm=TRUE) , lon=thelons[i])
              }
            else
              {
            
            LL1 = list(lat=MYBOT[j,4], lon=MYBOT[j,5])

          }

            j = which(thelons[i]==MYTOP[,1])
            
            if(length(j)<1)
              {
                LL2 = list(lat=max(MYTOP[,4], na.rm=TRUE) , lon=thelons[i])
              }
            else
              {
            
            LL2 = list(lat=MYTOP[j,4], lon=MYTOP[j,5])

          }

            
          ##   print(paste(sep=" ", LL1$lat[1]  ,LL1$lon[1], LL2$lat[1] , LL2$lon[1]) )

             if(any(is.na(c( LL1$lat[1], LL2$lat[1] ))) ) next
            
           ##### G = getgreatcirc(min(c( B1$lat, B3$lat))   ,thelons[i], LL$lat[1] , thelons[i], 1000)
            G =getgreatarc( LL1$lat[1]  ,LL1$lon[1], LL2$lat[1] , LL2$lon[1], 1000)
         ##    print(range(G$lat))
            gxy = GLOB.XY(G$lat , G$lon, proj)
            
           ## gxy = GLOB.XY(beelat$lat , rep(BLONbot[i], length=length(beelat$lat)), proj)

             gxy$x[gxy$x<botleft.x|gxy$x>topright.x] = NA
             gxy$y[gxy$y<botleft.y|gxy$y>topright.y] = NA
            
            lines(gxy$x, gxy$y,  col=col, lty=lty, lwd=lwd)
            
          }



       ### m1 = match( iBLATleft ,  iBLATright)
       ### thelats = iBLATleft[m1[!is.na(m1)]]

        thelats =  unique(c(iBLATleft ,  iBLATright) )

        ############################################
        for(i in 1:length(thelats))
          {


            j = which(thelats[i]==iBLATleft)
            
            LL1 = XY.GLOB( xleft[Fbeesleft[j]], yleft[Fbeesleft[j]] , proj)

            j = which(thelats[i]==iBLATright)
            
            LL2 = XY.GLOB(  xright[Fbeesright[j]], yright[Fbeesright[j]], proj)

            

          ##   G = getgreatcirc(thelats[i], min(c( B1$lon, B2$lon)), thelats[i]   , max(c( B3$lon, B4$lon)) , 10)
           ##  print(c(LL1$lon[1] , to=LL2$lon[1]))
            if(any(is.na(c(LL1$lon[1] , to=LL2$lon[1]))) ) next
            alons = seq(from=LL1$lon[1] , to=LL2$lon[1]  , length=1000)
            
            gxy = GLOB.XY(thelats[i] , alons, proj)
       
            
           ## gxy = GLOB.XY( rep(BLATleft[i], length=length(beelon$lon))   , beelon$lon , proj )

         ##    gxy$x[gxy$x<botleft.x|gxy$x>topright.x] = NA
          ##   gxy$y[gxy$y<botleft.y|gxy$y>topright.y] = NA
            
            
            lines(gxy$x, gxy$y, col=col, lty=lty, lwd=lwd)
          }
        

      }
############################################


  }

