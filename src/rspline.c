
#include <stdio.h>
#include <stdlib.h>
#include <math.h>


/*
  #ifdef MAC
  #include <sys/malloc.h>
  #else
  #include <malloc.h>
  #endif
*/



#define   NRANSI



/*
 * DGTSL GIVEN A GENERAL TRIDIAGONAL MATRIX AND A RIGHT HAND
 * SIDE WILL FIND THE SOLUTION.
 *
 * ON ENTRY
 *
 * n	INTEGER
 *	IS THE ORDER OF THE TRIDIAGONAL MATRIX.
 *
 * c	float(N)
 *	IS THE SUBDIAGONAL OF THE TRIDIAGONAL MATRIX.
 *	C(2) THROUGH C(N) SHOULD CONTAIN THE SUBDIAGONAL.
 *	ON OUTPUT C IS DESTROYED.
 *
 * d	float(N)
 *	IS THE DIAGONAL OF THE TRIDIAGONAL MATRIX.
 *	ON OUTPUT D IS DESTROYED.
 *
 * e	float(N)
 *	IS THE SUPERDIAGONAL OF THE TRIDIAGONAL MATRIX.
 *	E(1) THROUGH E(N-1) SHOULD CONTAIN THE SUPERDIAGONAL.
 *	ON OUTPUT E IS DESTROYED.
 *
 * b	float(N)
 *	IS THE RIGHT HAND SIDE VECTOR.
 *
 * ON RETURN
 *
 * b	IS THE SOLUTION VECTOR.
 *
 * INFO	INTEGER
 *	= 0 NORMAL VALUE.
 *	= K IF THE K-TH ELEMENT OF THE DIAGONAL BECOMES
 *	EXACTLY ZERO.  THE SUBROUTINE RETURNS WHEN
 *	THIS IS DETECTED.
 */

#define JM_ADD 0


#define XmapNewArray(T, N) \
        (T *)calloc((size_t)(N), sizeof(T))



#define XmapFree(P) \
        free((void *)(P))


double *alloc_dvec(long first, long last)
  {
  double *a;

  a = XmapNewArray(double, (last - first + 1 + JM_ADD));
  return(a - first + JM_ADD);
  }
void free_dvec(double *a, long first, long last)
  {

  XmapFree(a + first - JM_ADD);
  }

/* --- feature switches --- */

#define _POSIX_SOURCE 1

/* --- system headers --- */

/* --- local headers --- */


/* --- functions --- */

/** FUNC DEF */ int jtridiag(int n, double *c, double *d, double *e, double *b)
  {
  int  info = 0, i;
  double d1, d2;
  static int k, kb, kp1, nm1, nm2;
  static double t;

  c[1] = d[1];
  nm1 = n - 1;
  if(!(nm1 < 1))
    {
    d[1] = e[1];
    e[1] = 0.0;
    e[n] = 0.0;

    i = nm1;
    for (k = 1; k <= i; k++)
      {
      kp1 = k + 1;

      /* FIND THE LARGEST OF THE TWO ROWS */

      if(!((d1 = c[kp1], fabs(d1)) < (d2 = c[k], fabs(d2))))
	{
	/* INTERCHANGE ROW */

	t = c[kp1];
	c[kp1] = c[k];
	c[k] = t;
	t = d[kp1];
	d[kp1] = d[k];
	d[k] = t;
	t = e[kp1];
	e[kp1] = e[k];
	e[k] = t;
	t = b[kp1];
	b[kp1] = b[k];
	b[k] = t;
	}

      /* ZERO ELEMENTS */

      if(c[k] == 0.0) return(info = k);

      t = -c[kp1] / c[k];
      c[kp1] = d[kp1] + t * d[k];
      d[kp1] = e[kp1] + t * e[k];
      e[kp1] = 0.;
      b[kp1] += t * b[k];
      }
    }

  if(c[n] == 0.0) return(info = n);

  /* BACK SOLVE */

  nm2 = n - 2;
  b[n] /= c[n];
  if(n == 1) return(info);

  b[nm1] = (b[nm1] - d[nm1] * b[n]) / c[nm1];
  if(nm2 < 1) return(info);

  i = nm2;
  for(kb = 1; kb <= i; kb++)
    {
    k = nm2 - kb + 1;
    b[k] = (b[k] - d[k] * b[k + 1] - e[k] * b[k + 2]) / c[k];
    }
  return(info);
  }

/****************************************************************/
/****************************************************************/
/****************************************************************/
/****************************************************************/

/** FUNC DEF */ void jspl(double *x, double *y, int n, int ndiv, double *ex, double *ey)
  {
  double *t, *a, *b, *c, *a2, *b2, *c2, *Bx, *By, ttt, tt, tlen,
    *betax, *betay;
  int i, j, k;
/* fprintf(stderr, "in  rspline: input %d\n", n); */

/*  for(j=0; j<n; j++) fprintf(stderr, "%lf %lf\n", x[j], y[j]); */
/* fprintf(stderr, "###############  end input\n"); */



  t = alloc_dvec(1, n);


  for(i = 1; i <= n - 1; i++)
    {
    t[i + 1] =  sqrt(((x[i] - x[i - 1]) * (x[i] - x[i - 1])
		  + (y[i] - y[i - 1]) * (y[i] - y[i - 1])));

    }

  a = alloc_dvec(1, n);
  b = alloc_dvec(1, n);
  c = alloc_dvec(1, n);

  a2 = alloc_dvec(1, n);
  b2 = alloc_dvec(1, n);
  c2 = alloc_dvec(1, n);

  Bx = alloc_dvec(1, n);
  By = alloc_dvec(1, n);

  betax = alloc_dvec(1, 4);
  betay = alloc_dvec(1, 4);

  for(i = 2; i <= n - 1; i++)
    {
    a[i] = t[i + 1];
    c[i] = t[i];
    b[i] = 2 * (t[i] + t[i + 1]);

    Bx[i] = (3.0 * (t[i] * t[i] * (x[i] - x[i - 1]) + t[i + 1] * t[i + 1]
		    * (x[i - 1] - x[i - 2])) / (t[i] * t[i + 1]));
    By[i] = (3.0 * (t[i] * t[i] * (y[i] - y[i - 1]) + t[i + 1] * t[i + 1]
		    * (y[i - 1] - y[i - 2])) / (t[i] * t[i + 1]));
    }

  a[n] = 2;
  b[n] = 4;
  b[1] = 1;
  c[1] = 0.5;

  Bx[1] = 1.5 * (x[1] - x[0]) / t[2];
  Bx[n] = 6 * (x[n - 1] - x[n - 2]) / t[n];
  By[1] = 1.5 * (y[1] - y[0]) / t[2];
  By[n] = 6 * (y[n - 1] - y[n - 2]) / t[n];

  /* need to copy input here because routine destroys tridiagonal matrix  */

  for(i = 1; i <= n; i++)
    {
    a2[i] = a[i];
    b2[i] = b[i];
    c2[i] = c[i];
    }

  jtridiag(n, a, b, c, Bx);
  jtridiag(n, a2, b2, c2, By);

  k = 1;
	
  ex[0] = x[0];
  ey[0] = y[0];

  for(i = 1; i < n; i++)
    {
    tt = t[i + 1] * t[i + 1];
    ttt = t[i + 1] * t[i + 1] * t[i + 1];

    betax[1] = x[i - 1];
    betax[2] = Bx[i];
    betax[3] = (3 * (x[i] - x[i - 1]) / tt - 2 * Bx[i] / t[i + 1] - Bx[i + 1]
		/ t[i + 1]);
    betax[4] = (2 * (x[i - 1] - x[i]) / ttt + Bx[i] / tt + Bx[i + 1] / tt);

    betay[1] = y[i - 1];
    betay[2] = By[i];
    betay[3] = (3 * (y[i] - y[i - 1]) / tt - 2 * By[i] / t[i + 1] - By[i + 1]
		/ t[i + 1]);
    betay[4] = (2 * (y[i - 1] - y[i]) / ttt + By[i] / tt + By[i + 1] / tt);

    for(j = 1; j <= ndiv; j++)
      {
      tlen = j * t[i + 1] / ndiv;
      ex[k] = (betax[1] + betax[2] * tlen + betax[3] * tlen * tlen + betax[4]
	       * tlen * tlen * tlen);
      ey[k] = (betay[1] + betay[2] * tlen + betay[3] * tlen * tlen + betay[4]
	       * tlen * tlen * tlen);
      k++;
      }
    }

  free_dvec(t, 1, n);
  free_dvec(a, 1, n);
  free_dvec(b, 1, n);
  free_dvec(c, 1, n);

  free_dvec(a2, 1, n);
  free_dvec(b2, 1, n);
  free_dvec(c2, 1, n);

  free_dvec(Bx, 1, n);
  free_dvec(By, 1, n);

  free_dvec(betax, 1, 4);
  free_dvec(betay, 1, 4);
  }





/** FUNC DEF */ void   CALL_jspline(double *x, double *y, int  *n, int  *ndiv, double *ex, double *ey)
{
  int k, kdiv;

  k = *n;
  kdiv = *ndiv;

/*   fprintf(stderr, "%d %d\n", k, kdiv); */


 
  jspl(x, y, k, kdiv, ex, ey); 


 return;
}



