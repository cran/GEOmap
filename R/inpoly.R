`inpoly` <-
function(x,y, POK )
  {
G = rep(0,length(x))
for(i in 1:length(x))
{
A = list(x=x[i], y=y[i])
G[i] = inside(A , POK )

}
return(G)
}

