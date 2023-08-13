`inpoly` <-
  function(x,y, POK )
  {
    ##  
    kin = fields::in.poly(cbind(x, y) ,cbind(POK$x, y =POK$y) )

    G = rep(0,length(x))
    G[kin] = 1

    return(G)
  }

