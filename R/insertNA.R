insertNA<-function(y, ind)
  {
    wdiy = c(0, ind, length(y))
    Z = NULL
    for(j in 1:(length(wdiy)-1))
      {
        if((wdiy[j+1])<(wdiy[j]+1)) next
        ind = seq(from=(wdiy[j]+1), to=(wdiy[j+1]), by=1)
#### print(ind)
#### print(y[ind])
        Z = c(Z, y[ind], NA)
      }
    
    
    return(Z)
  }
