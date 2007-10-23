`subsetTOPO` <-
function(TOPO, ALOC)
{
  

  nn = names(ALOC)

  if(length(nn)<1)
    { return(NULL) }

  ilon = grep("lon", nn, ignore.case = TRUE)
  ilat = grep("lat", nn, ignore.case = TRUE)

  if(length(ilon)<1)   { return(NULL) }
  if(length(ilat)<1)   { return(NULL) }

  
  A = list(lat=ALOC[[ilat[1]]], lon=ALOC[[ilon[1]]], LAT=ALOC[[ilat[1]]], LON=fmod(ALOC[[ilon[1]]], 360) )
  
  
  nlats=  A$lat	
  
  DTOP = dim(TOPO)
  
  topolon = attr(TOPO, 'lon')
  topolat = attr(TOPO, 'lat')

  FLON = fmod(A$lon, 360)

  dlon = difflon(FLON[1], FLON[2])
  EL1 = FLON[1]+dlon$deg
  

####  wlats = which( topolat>nlats[1] & topolat<nlats[2] )
  wlats =  topolat>nlats[1] & topolat<nlats[2] 

  if(FLON[1]<360 & EL1>360)
    {
####   boundary crosses the 0-longitude and care must be taken

      ##  need to glue the two parts together

      mlon = fmod(mean(c(FLON[1], EL1)), 360)
      
      nlons = A$lon
      
      t1 = which(topolon>A$LON[1])
      t2 = which( topolon<A$LON[2])
      
      ax = c( topolon[t1]-360 , topolon[t2])
      ay = topolat[topolat>nlats[1]&topolat<nlats[2] ]
      PLON  = pretty(A$lon)
      PLON = c(min(A$lon), PLON[PLON>min(A$lon) & PLON<max(A$lon)], max(A$lon))

      
      mytop = TOPO[c(t1,t2)  , rev(topolat>nlats[1] & topolat<nlats[2] ) ]
      d = dim(mytop)
      
    }

  else
    {

      
      mlon = mean(A$LON)
      
      nlons = A$LON
      
      PLON  = pretty(A$LON)
      PLON = c(min(A$LON), PLON[PLON>min(A$LON) & PLON<max(A$LON)], max(A$LON))
      L = list(x=nlons, y = nlats)
      
      mytop = TOPO[topolon>L$x[1]&topolon<L$x[2]  , rev(topolat>L$y[1] & topolat<L$y[2] ) ]
      
      ax = topolon[topolon>L$x[1]&topolon<L$x[2]]
      ay = topolat[topolat>L$y[1]&topolat<L$y[2] ]
      d = dim(mytop)
      
    }

  return(list(z=mytop,x=ax, y=ay) )
  
}

