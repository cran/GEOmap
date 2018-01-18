`lamaz.eqarea` <-
function( phi1,  lam0,  phi,  lam, R=6371 )
{
  ### R is the radius of the sphere
  
  kprime = sqrt(2/(1+sin(phi1)*sin(phi)+cos(phi1)*cos(phi)*cos(lam-lam0) ) );
  
  x = R*kprime*cos(phi)*sin(lam-lam0);
  y = R*kprime*(cos(phi1)*sin(phi)-sin(phi1)*cos(phi)*cos(lam-lam0));
  
  return(list(x=x, y=y))
}


lamaz.inverse <- function (phi1, lam0, x, y, R=6371 ) 
{
    rho = sqrt(x^2 + y^2)/R
    cee = 2*asin( rho/2 )

    phi = asin( cos(cee)*sin(phi1) + y*sin(cee)*cos(phi1) )
    lam = lam0 + atan2( x*sin(cee) , rho*cos(phi1)*cos(cee) - y * sin(phi1)*sin(cee) )

    return(list(phi=phi, lam=lam) )

}
