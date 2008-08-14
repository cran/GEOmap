getgreatarc<-function( lat1, lon1,  lat2,  lon2, num)
{
### great circles
 
  phi1 = pi*(90-lat1)/180
  lam1 = pi*(lon1)/180
  phi2= pi*(90-lat2)/180
  lam2= pi*(lon2)/180

  
v1 = c(cos(lam1)* sin(phi1) , sin(lam1)* sin(phi1) ,      cos(phi1))
v2 = c(cos(lam2)* sin(phi2) , sin(lam2)* sin(phi2) ,      cos(phi2))

 a1 = acos( sum(v1*v2)  )

 ang1 = 180*a1/pi

v2v1 = v2-v1
dis = sqrt( sum(v2v1*v2v1) ) 

 dir = v2v1/dis

 pdis = seq(from=0, to=dis, length=num)
 
 g = diag(pdis) %*% cbind(rep(dir[1], num), rep(dir[2], num), rep(dir[3], num)) 


 pees =  cbind(rep(v1[1], num), rep(v1[2], num), rep(v1[3], num))+g
 
 spee = apply(pees*pees, 1, "sum")

 gee = sweep(pees, 1, spee, FUN="/")


 lat = acos(gee[,3])
 lon = atan2(gee[,2], gee[,1])

 LAT = 90-180*lat/pi
 LON = 180*lon/pi
   
return(list(lat=LAT, lon=LON))
 
}
