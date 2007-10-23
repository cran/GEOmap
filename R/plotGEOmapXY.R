`plotGEOmapXY` <-
function(MAP, LIM=c(-180, -90, 180, 90), PROJ=list(),  PMAT=NULL, add=TRUE, GRID=NULL,
                        GRIDcol=1, MAPcol=NULL, cenlon=0, shiftlon=0, linelty=1, linelwd=1, NUMB=FALSE, ...)
{
  ###  NUMB = add a number on the stroke to show which stroke it is for later modification
  ######   MAPcol will override the color in teh map data base.  Good for making BW figures
  if(missing(cenlon)) { cenlon=0 }
  if(missing(GRID)) { GRID=NULL }
    if(missing(GRIDcol)) { GRIDcol=1 }
  if(missing(PMAT)) { PMAT=NULL }
  if(missing(linelty)) { linelty=1 }
  if(missing(NUMB)) { NUMB=FALSE }
  if(missing(linelwd)) { linelwd=1 }
  if(missing(MAPcol)) { MAPcol=NULL }
  if(missing(shiftlon)) {  shiftlon=0 }
  if(missing(PROJ)) {

   PROJ = setPROJ(type=2, LAT0=median(MAP$POINTS$lat), LON0=median(MAP$POINTS$lon-shiftlon) , LATS=NULL, LONS=NULL, DLAT=NULL, DLON=NULL, FN =0)


  

  }



  
 ###  MAP$POINTS$lon = fmod( MAP$POINTS$lon, 360)

  if(missing(LIM))
    {
      lon = fmod(MAP$POINTS$lon-shiftlon, 360)
      
      LIMP=c( min(lon), min(MAP$POINTS$lat), max(lon), max(MAP$POINTS$lat))
      LIM=c( min(fmod(MAP$POINTS$lon, 360)), min(MAP$POINTS$lat), max(fmod(MAP$POINTS$lon, 360)), max(MAP$POINTS$lat))
    }
  else
    {

      
      if(!is.list(LIM))
        {
          LIMP=LIM
        }
      else
        {

          lon = fmod(LIM$lon-shiftlon, 360)
          lat = LIM$lat
          LIMP=c( min(lon), min(lat), max(lon), max(lat))
          LIM=c( min(fmod(lon, 360)), min(lat), max(fmod(lon, 360)), max(lat))
          
        }
    }
  
  

  LLlim = list(lat=LIM[c(2,4)], lon=LIM[c(1,3)])
  
  if(missing(add)) { add=FALSE }


  if(exists("PROJ.DATA")==FALSE)
    {
      setPROJ(type=2, LAT0=median(MAP$POINTS$lat), LON0=median(MAP$POINTS$lon-shiftlon) , LATS=NULL, LONS=NULL, DLAT=NULL, DLON=NULL, FN =0)
    }

  if(is.null(MAP$POINTS$z))
     {

       MAP$POINTS$z = rep(0, length(MAP$POINTS$lat))

     }

  if(is.null(MAP$POINTS$x))
    {
      MAPXY = GLOB.XY(MAP$POINTS$lat ,  fmod( MAP$POINTS$lon-shiftlon, 360) , PROJ )

      if(is.null(PMAT))
        {
          MAP$POINTS$x = MAPXY$x
          MAP$POINTS$y = MAPXY$y
        }
      else
        {
          
          tem = trans3d(MAPXY$x, MAPXY$y, MAP$POINTS$z , PMAT)
          MAP$POINTS$x = tem$x
          MAP$POINTS$y = tem$y
        }

      STRKXYLL = GLOB.XY( MAP$STROKES$LAT1,  fmod(MAP$STROKES$LON1-shiftlon, 360)  , PROJ )
      STRKXYUR = GLOB.XY( MAP$STROKES$LAT2,  fmod(MAP$STROKES$LON2-shiftlon, 360)  , PROJ )


      
      if(is.null(PMAT))
        {
          MAP$STROKES$x1 = STRKXYLL$x
          MAP$STROKES$y1 = STRKXYLL$y
        }
      else
        {
          tem = trans3d(STRKXYLL$x, STRKXYLL$y, rep(0, length(STRKXYLL$y)) , PMAT)

          MAP$STROKES$x1= tem$x
          MAP$STROKES$y1= tem$y
        }


      
      if(is.null(PMAT))
        {
          MAP$STROKES$x2 = STRKXYUR$x
          MAP$STROKES$y2 = STRKXYUR$y
        }
      else
        {

          tem = trans3d(STRKXYUR$x, STRKXYUR$y, rep(0, length(STRKXYUR$y)) , PMAT)

          MAP$STROKES$x2= tem$x
          MAP$STROKES$y2= tem$y
        }

    }
  
 ##  print(paste(sep=" ", "test", min(MAP$POINTS$x)))
 ##  print(LIM)


  
  xrange = diff(range(MAP$POINTS$x, na.rm=TRUE))
  yrange = diff(range(MAP$POINTS$y, na.rm=TRUE))

  Kstroke = length(MAP$STROKES$num)


  ###  if(exists("worldmap"))

  if(TRUE)
    {
      y1 = MAP$STROKES$LAT1
      y2 = MAP$STROKES$LAT2
      x1 =   fmod(MAP$STROKES$LON1, 360)
      x2 =   fmod(MAP$STROKES$LON2, 360)
      
      
      y3 = LIM[2]
      y4 = LIM[4]
      x3 =  fmod(LIM[1], 360)
      x4 =  fmod(LIM[3], 360)
      
      
      
      OUT = y1>=y4 | x1>=x4 | y2 <= y3 | x2 <= x3
      
      IN = which(!OUT)
    }
  else
    {
      IN = 1:length(MAP$STROKES$num)

    }

  minx=Inf; maxx=-Inf; miny=Inf; maxy=-Inf;
  
  ##  print(IN)
  if(length(IN)<1)
    {
      print("No map strokes in target")
      return(0)

    }

##############   this inserts NA where the lines cross the boundaries 
  for(i in IN)
    {
      
      j1 = MAP$STROKES$index[i]+1
      j2 = j1+MAP$STROKES$num[i]-1
      
      if(j1>0 & j2>0 & j2-j1 >0)
        {
          JEC = j1:j2
          x = MAP$POINTS$x[JEC]
          y = MAP$POINTS$y[JEC]
          x[MAP$POINTS$lon[JEC]<LLlim$lon[1] |  MAP$POINTS$lon[JEC]>LLlim$lon[2] ] = NA
          y[MAP$POINTS$lat[JEC]<LLlim$lat[1] |  MAP$POINTS$lat[JEC]>LLlim$lat[2] ] = NA
          minx = min(c(minx,x), na.rm=TRUE)
          maxx = max(c(maxx,x), na.rm=TRUE)
          miny = min(c(miny,y), na.rm=TRUE)
          maxy = max(c(maxy,y), na.rm=TRUE)
        }
      else
        {
          next
        }
      
    }
##############   this replaces the colors with a fixed color
  if(!is.null(MAPcol))
    {

      MAP$STROKES$col = rep(MAPcol, length=length(MAP$STROKES$col))

    }

  ##############

  if(add==FALSE)
    {
###R1 = range(c(MAP$STROKES$x1[IN],MAP$STROKES$x2[IN]))
###R2 = range(c(MAP$STROKES$y1[IN], MAP$STROKES$y2[IN]))
      plot(c(minx, maxx) , c(miny,maxy), asp=TRUE, type='n', ...)
      
### plot(c(MAP$STROKES$x1[IN],MAP$STROKES$x2[IN]) , c(MAP$STROKES$y1[IN], MAP$STROKES$y2[IN]), asp=TRUE, type='n', ...)
###print(c(R1, R2))
    }

  
  if(!is.null(GRID))
    {
      addLLXY(GRID$lats, GRID$lons, PMAT=PMAT, GRIDcol=GRIDcol, LABS=0, BORDER=0 , PROJ=PROJ )
    }
  
  for(i in IN)
    {
      j1 = MAP$STROKES$index[i]+1
      j2 = j1+MAP$STROKES$num[i]-1
      
      if( (j1>0 & j2>0 & j2-j1 >0))
        {
          JEC = j1:j2
        }
      else
        {
          next
        }
      
      ###  print(paste(sep=' ',"----------------------", i, j1,j2))

      if(NUMB==TRUE)
        {
          points(MAP$POINTS$x[j1], MAP$POINTS$y[j1])
          text(MAP$POINTS$x[j1], MAP$POINTS$y[j1], labels=i, pos=3)
        }

      
      if(MAP$STROKES$style[i]==1)
        {
          points(MAP$POINTS$x[JEC], MAP$POINTS$y[JEC], col=MAP$STROKES$col[i])
        }

      
      if(MAP$STROKES$style[i]==2)
        {
          x = MAP$POINTS$x[JEC]
          y = MAP$POINTS$y[JEC]

         
          
          xd = abs(diff(c(x, x[1])))
          ww = which(xd>0.9*xrange)
          if(!is.null(ww) & length(ww)>0 )
            {
              print(paste(sep=' ', "################", i, length(x), length(ww)))
              print(x)
              ##  print(ww)
              wdi = c(0, ww, length(x))
              
              print(wdi)
              for(j in 1:(length(wdi)-1))
                {
                  if((wdi[j+1])<(wdi[j]+1)) next
                  ind = seq(from=(wdi[j]+1), to=(wdi[j+1]), by=1)
                  lines(x[ind], y[ind], col=MAP$STROKES$col[i], lty=linelty, lwd=linelwd)
                }

              
            }
          else
            {
              x[MAP$POINTS$lon[JEC]<LLlim$lon[1] |  MAP$POINTS$lon[JEC]>LLlim$lon[2] ] = NA
              y[MAP$POINTS$lat[JEC]<LLlim$lat[1] |  MAP$POINTS$lat[JEC]>LLlim$lat[2] ] = NA
              
              lines(x, y, col=MAP$STROKES$col[i], lty=linelty, lwd=linelwd)
            } 
        }

      if(MAP$STROKES$style[i]==3)
        {

          x = MAP$POINTS$x[JEC]
          y = MAP$POINTS$y[JEC]

         ##### x[MAP$POINTS$lon[JEC]<LLlim$lon[1] |  MAP$POINTS$lon[JEC]>LLlim$lon[2] ] = NA
         ##### y[MAP$POINTS$lat[JEC]<LLlim$lat[1] |  MAP$POINTS$lat[JEC]>LLlim$lat[2] ] = NA

          
          polygon(x, y, border=FALSE, col=MAP$STROKES$col[i])



          
        }



      
      ##  locator()

    }

  invisible(IN)
  

}

