SELGEOmap<-function(MAP , ncut=3, acut=c(0,100000), proj=NULL, LIM=NULL )
  {
########  calculate the area of map elements in GEOmap
    if(missing(ncut)) ncut=3 
    if(missing(proj)) proj=NULL 
    if(missing(acut)) acut=NULL
    if(missing(LIM))   LIM=NULL

    if(!require(splancs)) { print("NEED splancs"); return(NULL) }

    NEWMAP = list(STROKES=list(nam=NULL, num=NULL, index=NULL,
                    col=NULL, style=NULL, code=NULL,
                    LAT1=NULL, LAT2=NULL, LON1=NULL, LON2=NULL),
      POINTS= list(lat=NULL, lon=NULL))

    if(!is.null(proj) & !is.null(LIM))
      {
        LLlim = list(lat=LIM[c(2,4)], lon=LIM[c(1,3)])
        
        STRKXYLL = GLOB.XY( MAP$STROKES$LAT1,  MAP$STROKES$LON1  , proj )
        STRKXYUR = GLOB.XY( MAP$STROKES$LAT2,  MAP$STROKES$LON2  , proj )
        MAP$STROKES$x1 = STRKXYLL$x
        MAP$STROKES$y1 = STRKXYLL$y
        MAP$STROKES$x2 = STRKXYUR$x
        MAP$STROKES$y2 = STRKXYUR$y
        
        y1 = MAP$STROKES$y1
        y2 = MAP$STROKES$y2
        x1 = MAP$STROKES$x1
        x2 = MAP$STROKES$x2
        
        XYLIM =  GLOB.XY(LLlim$lat,LLlim$lon, proj)
        LLlim$x = XYLIM$x
        LLlim$y = XYLIM$y
        
        y3 =XYLIM$y[1]
        y4 =XYLIM$y[2]
        x3 =XYLIM$x[1]
        x4 = XYLIM$x[2]
        
        OUT = y1>=y4 | x1>=x4 | y2 <= y3 | x2 <= x3
        
      }
    else
      {

        OUT = rep(FALSE, length=length(MAP$STROKES$num))

        }

    Kmap = 0
    index1 = 0
    for(i in 1:length(MAP$STROKES$num))
      {
        useit = FALSE
        OKarea  = FALSE
        OKnum = FALSE
        if(MAP$STROKES$num[i] > ncut)
          {
            
            j1 = MAP$STROKES$index[i] + 1
            j2 = j1 + MAP$STROKES$num[i] - 1
            JEC = j1:j2
            lon = MAP$POINTS$lon[JEC]
            lat = MAP$POINTS$lat[JEC]

            if(is.null(proj))
              {
                PROJ = setPROJ(type=2, LAT0=median(lat), LON0=median(lon))
              }
            else
              {
                PROJ = proj

              }

            X = GLOB.XY(lat, lon, PROJ)
            POL = cbind(X$x, X$y)
            
            OKnum = TRUE
            if(!is.null(acut))
              {
                cgAREA = areapl(POL)

                if(cgAREA >= acut[1] & cgAREA <= acut[2]){
                  OKarea = TRUE
                }
              }

          }

        useit = OKarea & OKnum & !OUT[i]

        if(useit)
          {
            Kmap = Kmap+1
            NEWMAP$STROKES$nam = c(NEWMAP$STROKES$nam, MAP$STROKES$nam[i])
            NEWMAP$STROKES$num = c(NEWMAP$STROKES$num, MAP$STROKES$num[i])
            NEWMAP$STROKES$index= c(NEWMAP$STROKES$index, index1)
            index1 = index1 + MAP$STROKES$num[i]
            NEWMAP$STROKES$col= c(NEWMAP$STROKES$col, MAP$STROKES$col[i])
            NEWMAP$STROKES$style= c(NEWMAP$STROKES$style, MAP$STROKES$style[i])
            NEWMAP$STROKES$code = c(NEWMAP$STROKES$code , MAP$STROKES$code[i])
            
            NEWMAP$STROKES$LAT1=c(NEWMAP$STROKES$LAT1 ,     MAP$STROKES$LAT1[i])
            NEWMAP$STROKES$LAT2=c(NEWMAP$STROKES$LAT2 ,    MAP$STROKES$LAT2[i])
            NEWMAP$STROKES$LON1=c(NEWMAP$STROKES$LON1 ,    MAP$STROKES$LON1[i])
            NEWMAP$STROKES$LON2=c(NEWMAP$STROKES$LON2 , MAP$STROKES$LON2[i])
            
            NEWMAP$POINTS$lon = c(NEWMAP$POINTS$lon, lon)
            NEWMAP$POINTS$lat = c(NEWMAP$POINTS$lat, lat)
            
          }



      }

    invisible(NEWMAP)


  }


