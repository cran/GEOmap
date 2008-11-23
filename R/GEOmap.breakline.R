GEOmap.breakline<-function(Z, ww)
{
  ########   ww is a list of locations where there are
  #######   NA
  
  nww = length(ww)


  newx = list()
  newy = list()

  if(nww<1) return(list(newx=Z$x, newy=Z$y) )

  ## ww = c(0, ww, length(Z$x)+1)
####  build up a set of strokes separated by NA's
  
  j = 1
  for(i in seq(from=1, to=nww, by=1)  )
    {

      i2 = (ww[i])

      
      newx[[i]] = Z$x[j:i2]
      newy[[i]] =  Z$y[j:i2]
      j = ww[i]+1
    }

  i2 = length(Z$x)
  j=ww[i]+1
  i = i+1
  newx[[i]] = Z$x[j:i2]
  newy[[i]] = Z$y[j:i2]


  return(list(newx=newx, newy=newy))

}
