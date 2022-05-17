`setPROJ` <-
    function(type=1, LAT0=0, LON0=0 ,LAT1=0,  LAT2=0,LATS=NULL,
             LONS=NULL, DLAT=NULL, DLON=NULL,FE=0,FN=0, IDATUM=1 )
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

    #### these projections are built in to GEOmap:
    types = c("none", "merc.sphr", "utm.sphr", "lambert.cc", "stereo.sphr",
        "utm.elps", "equid.cyl" , "utm.wgs84", "UTM",  "OLDGCLC" )
    typnums = c(0,1,2,3,4,5,6, 7, 8,  99 )
    ##    cbind(types, typnums)

##########  determine numeric value of type
    
    if(is.character(type))
      {
          itype=which(type==types)
          atype = typnums[itype]
          
          if(length(atype)<1)
              {
                  #####  check to see if these functions exist locally
                  projxy = paste(type, '.xy', sep='')
                  projll = paste(type, '.ll', sep='')
                  
                  myenv = environment()
                  if (!is.null( get0(projxy, envir =myenv )) &
                      !is.null( get0(projll, envir =myenv )) )
                      {
####  these two functions exist, add to the list
                          types = c(types , type)
                          typnums = c(typnums, max(typnums)+1)
                          itype=which(type==types)
                          
                      }
                  
              }
          else
              {
                  type = typnums[itype]
              }
                  
                  
                  
              }
          else
              {
                  itype=which(type==typnums)
                  
              }
#### type is not found, see if it exists and register it:
    
    
    
    name = types[ itype]
    DATUM = list(name=NA, a=NA, b=NA, flat=NA, use=NA )

    datums =  DATUMinfo()

    if(type==7)
      {
        LON0=LON0
          IDATUM = 1
         DATUM = list(name=datums[[1]][IDATUM],
           a=datums[[2]][IDATUM],
           b=datums[[3]][IDATUM],
           flat=datums[[4]][IDATUM],
           use=datums[[5]][IDATUM] )
      
      }
    else
      {
        LON0=RPMG::fmod(LON0, 360)
        
      }

    if(type==8)
      {
        LON0=LON0
          DATUM = list(name=datums[[1]][IDATUM],
           a=datums[[2]][IDATUM],
           b=datums[[3]][IDATUM],
           flat=datums[[4]][IDATUM],
           use=datums[[5]][IDATUM] )
       
      }
    else
      {
        LON0=RPMG::fmod(LON0, 360)
        
      }



    
    MAPconstants()
    
    PROJ.DATA=list(type=type, LAT0=LAT0, LON0=LON0,LAT1=LAT1, LAT2=LAT2,
			LATS=LATS, LONS=LONS, DLAT=DLAT, DLON=DLON, FE=FE,FN=FN, name=name, DATUM=DATUM )



    

    return(PROJ.DATA)
    
  }

