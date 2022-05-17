`XY.GLOB` <-
function(x, y, PROJ.DATA)
  {

    if(PROJ.DATA$type==0)
      {
        
        LL = list(lon=x , lat=y)
          return(LL)
      }
    if(PROJ.DATA$type==1)
      {
          LL = merc.sphr.ll(PROJ.DATA$LON0 , x , y)
            return(LL)
      }
    if(PROJ.DATA$type==2)
      {
          LL = utm.sphr.ll( x , y, PROJ.DATA)
            return(LL)
      }
    if(PROJ.DATA$type==3)
      {
          LL = lambert.cc.ll( x , y, PROJ.DATA)
            return(LL)
      }
    if(PROJ.DATA$type==4)
      {
          LL = stereo.sphr.ll( x , y, PROJ.DATA)
            return(LL)
      }
    if(PROJ.DATA$type==5)
      {
          LL = utm.elps.ll( x , y, PROJ.DATA)
            return(LL)
      }
    if(PROJ.DATA$type==6)
      {
          LL = equid.cyl.ll(PROJ.DATA$LON0 ,PROJ.DATA$LAT0 , x , y)
            return(LL)
      }
    if(PROJ.DATA$type==7)
      {
          LL = utm.wgs84.ll( x , y, PROJ.DATA)
            return(LL)
      }
    
    if(PROJ.DATA$type==99)
      {
          LL = lcgc(PROJ.DATA$LAT0, PROJ.DATA$LON0,  x , y)
            return(LL)
    }

    
  if(is.character(PROJ.DATA$type))
        {
#####  check to see if these functions exist locally
           ## print(paste('Character type ', PROJ.DATA$type) )
            projll = paste(PROJ.DATA$type, '.ll', sep='')
            myenv = environment()
            if (!is.null( rfun<-get0(projll, envir =myenv )) )
                {

                    LL =  rfun( x, y, PROJ.DATA)
                    return(LL)
                }
        }
    
    
    

    return(LL)

  }

