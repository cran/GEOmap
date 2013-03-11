`subsetTOPO` <-
function(TOPO, ALOC, PROJ, nx=500, ny=500, nb = 4, mb = 4, hb = 8)
{
  
#################   extract topographic information from ETOPO5 data base
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


 LLM =  meshgrid( topolon, topolat )



GG =   GLOB.XY( LLM$y, LLM$x, PROJ)

  
AXY =   GLOB.XY(A$lat, A$lon , PROJ)

WXY =   (GG$x>=AXY$x[1] & GG$x<=AXY$x[2] & GG$y>=AXY$y[1] & GG$y<=AXY$y[2] )
  
 ex =  as.vector(GG$x[WXY])
  why = as.vector(GG$y[WXY])


 ZIP =  t( TOPO[  ,dim(TOPO)[2]:1  ] )

  
  zee = as.vector(ZIP[WXY])

  
    xo = seq(from=min(ex), to=max(ex), length=nx)
   yo = seq(from=min(why), to=max(why) , length=ny)

  
## IZ = interp(x=GXY$x , y=GXY$y,  z=t(ZZ2$z)  , xo=xo, yo=yo, extrap=FALSE)

 DF = cbind(x=ex , y=why ,  z=zee)

  
    IZ = mba.surf(DF, nx, ny, n = nb, m = mb, h = hb, extend=TRUE)$xyz.est

  return(list(z=IZ$z,x=xo, y=yo) )
  
}

