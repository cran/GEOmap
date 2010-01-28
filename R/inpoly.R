`inpoly` <-
  function(x,y, POK )
  {
    require(splancs)
    kin = inout(cbind(x, y) ,cbind(POK$x, y =POK$y), bound=TRUE)

    G = rep(0,length(x))
    G[kin] = 1

    return(G)
  }

