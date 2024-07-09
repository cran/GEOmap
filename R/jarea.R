jarea <- function(L)
{
####  positive for right-hand polygons (CCW, counter-clockwise)
####  negative for the reverse (clockwise)
######  beef up the polygon
    if(!is.list(L) )
    {
        ##### if input L
        J = L
        L = list(x=J[,1], y=J[,2])
        }

    
    K = list(x=c(L$x, L$x[1], L$x[2]), y=c(L$y, L$y[1], L$y[2]) )
    
    N = length(K$x)-1
    i = 2:N
    V = K$x[i]*( K$y[i+1] - K$y[i-1] )
    return( sum(V)/2 )
}

