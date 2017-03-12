#include <R_ext/Rdynload.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/RS.h>
#include <stdlib.h> // for NULL

void   CALL_polydistpoint(double *x, double *y, int  *n, double *ex, double *ey, double *dis );

void   CALL_jspline(double *x, double *y, int  *n, int  *ndiv, double *ex, double *ey);



static const  R_CMethodDef CEntries[] = {
        {"CALL_polydistpoint", (DL_FUNC) &CALL_polydistpoint, 6},
      {"CALL_jspline", (DL_FUNC) &CALL_jspline, 6},
    {NULL, NULL, 0}
     };


   void
     R_init_RSEIS(DllInfo *dll)
     {
        R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
        R_useDynamicSymbols(dll, FALSE);  
     }
