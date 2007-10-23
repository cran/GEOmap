
geoarea<-function(MAP, proj, ncut=10)
  {
    if(missing(ncut)) ncut=10 

    if(!require(splancs)) { print("NEED splancs"); return(NULL) }

    cgAREA=rep(NA,length(MAP$STROKES$num)) 

    for(i in 1:length(MAP$STROKES$num))
      {

        if( MAP$STROKES$style[i] == 3 &  MAP$STROKES$num[i] > ncut)
          {
            j1 = MAP$STROKES$index[i] + 1
            j2 = j1 + MAP$STROKES$num[i] - 1
            JEC = j1:j2
            lon = MAP$POINTS$lon[JEC]
            lat = MAP$POINTS$lat[JEC]
            
            X = GLOB.XY(lat, lon, proj)
            POL = cbind(X$x, X$y)

            cgAREA[i] = areapl(POL)

          }
        else
          {
            cgAREA[i] = NA
          }




      }

    return(cgAREA)


  }
