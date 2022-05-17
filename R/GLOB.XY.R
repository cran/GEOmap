`GLOB.XY` <-
function(LAT, LON, PROJ.DATA)
  {


    if(PROJ.DATA$type==0)
      {
          XY = list(x=LON, y=LAT)
          return(XY)
      } 
    if(PROJ.DATA$type==1)
      {
          XY = merc.sphr.xy(PROJ.DATA$LON0 , LAT, LON)
          return(XY)
      }
    if(PROJ.DATA$type==2)
      {
          XY = utm.sphr.xy( LAT, LON, PROJ.DATA)
          return(XY)
      }
    if(PROJ.DATA$type==3)
      {
          XY = lambert.cc.xy( LAT, LON, PROJ.DATA)
          return(XY)
      }
    if(PROJ.DATA$type==4)
      {
          XY = stereo.sphr.xy( LAT, LON, PROJ.DATA)
          return(XY)
      }
    if(PROJ.DATA$type==5)
      {
          XY = utm.elps.xy( LAT, LON, PROJ.DATA)
          return(XY)
      }
    if(PROJ.DATA$type==6)
      {
          XY = equid.cyl.xy(PROJ.DATA$LON0 ,PROJ.DATA$LAT0 , LAT, LON )
          return(XY)
      }
    if(PROJ.DATA$type==7)
      {
          XY = utm.wgs84.xy( LAT, LON, PROJ.DATA)
          return(XY)
      }



    
     if(PROJ.DATA$type==99)
      {
          XY =  gclc(PROJ.DATA$LAT0, PROJ.DATA$LON0, LAT, LON)
          return(XY)
    }


    if(is.character(PROJ.DATA$type))
        {
#####  check to see if these functions exist locally
           ## print(paste('Character type ', PROJ.DATA$type) )
            projxy = paste(PROJ.DATA$type, '.xy', sep='')
            myenv = environment()
            if (!is.null( rfun<-get0(projxy, envir =myenv )) )
                {

                    XY =  rfun( LAT, LON, PROJ.DATA)
                    return(XY)
                }
        }
    
    
   
    
    return(XY)
    
  }

