#include <stdio.h>
#include <math.h>


#define JMAX(a, b) ((a) > (b) ? (a) : (b))
#define JMIN(a, b) ((a) < (b) ? (a) : (b))


int  pline(double x1, double y1,double x2,double y2,double ex, double ey,
	   double *dis, double *zee , double *dee, double *px, double *py )
{
 /*  ##########   get the shortest distanvce to a line segment */
    double dx1, dy1, gx, gy, hx, hy;
    double d1,d2,d3;
    double cs, sn;


  dx1 = x2-x1;
  dy1 = y2-y1;
  gx = ex-x1;
  gy = ey-y1;
  hx = ex-x2;
  hy = ey-y2;

  d1 =sqrt(dx1*dx1 + dy1*dy1);



  d2 = sqrt(gx*gx + gy*gy);
  d3  = sqrt(hx*hx + hy*hy);

if(d1==0 || d2==0)
  {
    *dis = d2;
    *px = x1;
    *py = y1;
    *zee=0;
    *dee = *dis;

    return(3);

  }




 cs = (dx1*gx +dy1*gy) / (d1*d2);
 sn = (dx1*gy - gx*dy1)/(d1*d2);

 *zee = d2*cs;
 *dee = fabs(d2*sn);

if(*zee<0 || *zee>d1) {
  if(d2<d3)
    {
      *dis = d2;
      *px = x1;
      *py = y1;


    }
  else
    {
      *dis=d3;
      *px = x2;
      *py= y2 ;


    }

  return(2);

  
}
else {
  *dis = *dee;
  *px = x1+ *zee*dx1/d1;
  *py = y1+ *zee*dy1/d1;
}


 return(1);

}


/** FUNC DEF */ void   CALL_polydistpoint(double *x, double *y, int  *n, double *ex, double *ey, double *dis )
{
  int k;

  int i; 
 
  

  /* find the distance of a point from the perimeter of a polygon  **/

  double dee, zee;
  double  px1,py1;
  double adis, thedis;
 

  k = *n;



  i = 0; 
 pline(x[i], y[i], x[i+1], y[i+1], *ex, *ey, &adis, &dee, &zee, &px1, &py1 );

 thedis = adis;

  for(i=0; i<(k-1); i++)
    {

      pline(x[i], y[i], x[i+1], y[i+1], *ex, *ey, &adis, &dee, &zee, &px1, &py1 );
    
      if(adis<thedis) {thedis=adis;}
    }

  if(x[0]!=x[k-1] && y[0]!=y[k-1])
    {
      
      pline(x[k-1], y[k-1], x[0], y[0], *ex, *ey, &adis, &dee, &zee, &px1, &py1 );
    
      if(adis<thedis) {thedis=adis;}

    }
  
  *dis = thedis;

  return;
}





