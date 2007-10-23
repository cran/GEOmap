`boundGEOmap` <-
function(MAP, NEGLON=FALSE)
{
  if(missing(NEGLON)) { NEGLON=FALSE }
  ###############  if NEGLON=FALSE, convert all negative lons to positive
  ###########  if true, allow neg lons to stay negative
  ## the the bounds of strokes in a GEOmap
  
 Kstroke = length(MAP$STROKES$num)

 LAT1 = rep(NA, length=Kstroke)
 LAT2 = rep(NA, length=Kstroke)
 LON1 = rep(NA, length=Kstroke)
 LON2 = rep(NA, length=Kstroke)
 
  for(i in 1:Kstroke)
    {
      j1 = MAP$STROKES$index[i]+1
      j2 = j1+MAP$STROKES$num[i]-1

      blon  = range(MAP$POINTS$lon[j1:j2])
      blat  = range(MAP$POINTS$lat[j1:j2])
      LAT1[i] = blat[1]
      LAT2[i] = blat[2]
      LON1[i] = blon[1]
      LON2[i] = blon[2]
      
    }

 MAP$STROKES$LAT1=LAT1
 MAP$STROKES$LAT2=LAT2
 MAP$STROKES$LON1=LON1
 MAP$STROKES$LON2=LON2

  if(NEGLON==FALSE)
    {

      MAP$STROKES$LON1[MAP$STROKES$LON1<0] = fmod(MAP$STROKES$LON1[MAP$STROKES$LON1<0], 360)
      MAP$STROKES$LON2[MAP$STROKES$LON2<0] = fmod(MAP$STROKES$LON2[MAP$STROKES$LON2<0], 360)
      MAP$POINTS$lon[MAP$POINTS$lon<0] = fmod(MAP$POINTS$lon[MAP$POINTS$lon<0] , 360)

    }

 return(MAP)
}

