`setPROJ` <-
function(type=1, LAT0=0, LON0=0 ,LAT1=0,  LAT2=0,LATS=NULL, LONS=NULL, DLAT=NULL, DLON=NULL,FE=0,FN=0)
  {
    if(missing(type)) { type = 1 }
    if(missing(LAT0)) { LAT0 = 0 }
    if(missing(LON0)) { LON0 = 0 }
    if(missing(LAT1)) { LAT1 = 0 }
    if(missing(LAT2)) { LAT2 = 0 }
    
    if(missing(DLAT)) { DLAT = 1 }
    if(missing(DLON)) { DLON = 1 }
    
    if(missing(LATS)) { LATS =  list(S=LAT0-DLAT, N=LAT0+DLAT) }
    if(missing(LONS)) { LONS =  list(E=LON0-DLON, W=LON0+DLON) }
    if(missing(FE)) { FE=0}	
    if(missing(FN)) { FN=0}

    types = c("none", "merc.sphr", "utm.sphr", "lambert.cc", "stereo.sphr", "utm.elps", "equid.cyl" , "utm.wgs84", "OLDGCLC" )
    typnums = c(0,1,2,3,4,5,6, 7, 99 )
    
    if(is.character(type))
      {
        itype=which(type==types)
        type = typnums[itype]
        
      }
    else
      {
        itype=which(type==typnums)

      }
    
    
    name = types[ itype]

    if(type==7)
      {
        LON0=LON0
      }
    else
      {
        LON0=RPMG::fmod(LON0, 360)
      }
    
    MAPconstants()
    
    PROJ.DATA=list(type=type, LAT0=LAT0, LON0=LON0,LAT1=LAT1, LAT2=LAT2,
			LATS=LATS, LONS=LONS, DLAT=DLAT, DLON=DLON, FE=FE,FN=FN, name=name )

    return(PROJ.DATA)
    
  }

