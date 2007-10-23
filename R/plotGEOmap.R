`plotGEOmap` <-
function(MAP, LIM=c(-180, -90, 180, 90) , shiftlon=0, add=TRUE , NUMB=FALSE , SEL=NULL, MAPcol=NULL, PLOT=TRUE, ...)
{
  if(missing(add)) { add=FALSE }
  if(missing(NUMB)) { NUMB=FALSE }
  if(missing(shiftlon)) {  shiftlon=0 }
  if(missing(SEL)) {  SEL=NULL }
  if(missing(PLOT)) {  PLOT=TRUE }
  if(missing(MAPcol)) { MAPcol=NULL  }
  
  
  if(missing(LIM))
    {
      lon = fmod(MAP$POINTS$lon-shiftlon, 360)
      
      LIMP=c( min(lon), min(MAP$POINTS$lat), max(lon), max(MAP$POINTS$lat))
      LIM=c( min(fmod(MAP$POINTS$lon, 360)), min(MAP$POINTS$lat), max(fmod(MAP$POINTS$lon, 360)), max(MAP$POINTS$lat))
    }
  else
    {
      if(is.list(LIM))
        {
          
          lon = fmod(LIM$lon-shiftlon, 360)
          lat = LIM$lat
          LIMP=c( min(lon), min(lat), max(lon), max(lat))
          LIM=c( min(fmod(lon, 360)), min(lat), max(fmod(lon, 360)), max(lat))
          
        }
      else
        {
          LIMP=LIM
        }
      
    }
  
if(!is.null(MAPcol)) {  MAP$STROKES$col = rep(MAPcol, length(MAP$STROKES$col))  }
###  determine stroke inclusion

###  (x2>=x3)&&(x4>=x1)&& (y2>=y3)&&(y4>=y1)

  y1 = MAP$STROKES$LAT1
  y2 = MAP$STROKES$LAT2
  x1 = fmod(MAP$STROKES$LON1, 360)
  x2 = fmod(MAP$STROKES$LON2, 360)

  y3 = LIM[2]
  y4 = LIM[4]
  
  x3 = fmod(LIM[1], 360)
  x4 = fmod(LIM[3], 360)
  

  
  OUT = y1>=y4 | x1>=x4 | y2 <= y3 | x2 <= x3

  IN = which(!OUT)
  
 ###   MAP$STROKES$LAT1>=LIM[1] 
     
##########    for selecting parts of the map
  if(!is.null(SEL))
    {

      slin = which(IN %in%SEL)
      if(length(slin)>=1)
        { IN = IN[slin] }
    }
  
 ####     Kstroke = length(MAP$STROKES$num)
  ####   for(i in 1:Kstroke)


    if(add==FALSE)
    {
      ##  plot(MAP$POINTS$lon, MAP$POINTS$lat, type='n')
      ##  xlab="Lon", ylab="Lat",
      ## if(is.null(xlab)) { xlab="Lon" }
      
      plot(fmod(MAP$POINTS$lon-shiftlon, 360), MAP$POINTS$lat, xlim=c(LIMP[1], LIMP[3])  , ylim=c(LIM[2], LIM[4]),
           type='n', xlab="lon", ylab='lat', axes=FALSE,  ...)

      if(PLOT==FALSE) { return(0) }
      axis(2)
      
      pp = axTicks(1)

      xlabs = fmod(pp, 360)
      axis(1, at=pp, labels = xlabs)

      xlabs[xlabs>180 & xlabs<=359.99] = xlabs[xlabs>180 & xlabs<=359.99]-360
      axis(3, at=pp, labels =xlabs )

      
      box()
      
      
    }

  if(length(IN)<1) return(0)

  ###  print(IN)

     for(i in IN)
    {

      
      j1 = MAP$STROKES$index[i]+1
      j2 = j1+MAP$STROKES$num[i]-1

      LONS = fmod(MAP$POINTS$lon[j1:j2]-shiftlon, 360)

       if(NUMB==TRUE)
        {
          points(LONS[1], MAP$POINTS$lat[j1], pch='.', col="red")
          text( LONS[1], MAP$POINTS$lat[j1], labels=i, pos=3)
        }


      if(MAP$STROKES$style[i]==1)
        {
          points(LONS, MAP$POINTS$lat[j1:j2], col=MAP$STROKES$col[i])
        }

      
      if(MAP$STROKES$style[i]==2)
        {
          mline = LONS
          dline = c(0, abs(diff(mline)))
          mline[dline>100] = NA
          lines( mline, MAP$POINTS$lat[j1:j2], col=MAP$STROKES$col[i])
        }

      if(MAP$STROKES$style[i]==3)
        {
          polygon(LONS, MAP$POINTS$lat[j1:j2], border=FALSE, col=MAP$STROKES$col[i])
        }



      
      ##  locator()

    }

}

