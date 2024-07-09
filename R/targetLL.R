targetLL <-
function(sta, rdist=100)
{

 
  ###  get center for a projection

  mlat = median(sta$lat)
  mlon = median(sta$lon)

  proj =  setPROJ(2, LAT0 =mlat, LON0 =mlon )


    
message(paste(c(mlat, mlon) , collapse=' ') )


  Jlat = range(sta$lat)

  
  message(paste(Jlat  , collapse=' ') )

  Jlon = range(sta$lon)

 
  message(paste(Jlon  , collapse=' ') )
  
  labs = c("LL", "LR", "UL", "UR" )

  TARG = XY.GLOB(c(-rdist, rdist, -rdist, rdist ) , c(-rdist, -rdist, rdist, rdist  ), proj)

  A = cbind( TARG$lon, TARG$lat )

  blon = TARG$lon
  blon[TARG$lon>180] = blon-360
  B = cbind( blon , TARG$lat )
  rownames(A)<-labs
  rownames(B)<-labs

  
  write.table(A, quote = FALSE,col.names =FALSE )
  message("\n")
  
   write.table(B, quote = FALSE, col.names =FALSE)

  invisible(list(A=A, B=B, mlat=mlat, mlon=mlon,   Jlat =Jlat, Jlon =Jlon, proj=proj))
  

}

