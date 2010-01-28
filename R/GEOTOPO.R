`GEOTOPO` <-
  function(TOPO, PLOC, PROJ, calcol=NULL, npoints=500, PLOT=TRUE)
  {


    if(missing(calcol)) {  CCOL = settopocol(); calcol = CCOL$calcol }
    if(missing(npoints)) { npoints=500  }
    if(missing(PLOT)) { PLOT=TRUE }

    if(is.null(calcol)) {  CCOL = settopocol(); calcol = CCOL$calcol  }



    cat("Extracting from Data Base....please wait....", file="", sep="\n")
    
    if(TRUE)
      {
###  here make adjustments so the topo part is square, not curved
###  to do this extend the borders of extraction

        nn = names(PLOC)
        
        ilon = grep("lon", nn, ignore.case = TRUE)
        ilat = grep("lat", nn, ignore.case = TRUE)
        
        if(length(ilon)<1)   { return(NULL) }
        if(length(ilat)<1)   { return(NULL) }

        
        A = list(lat=PLOC[[ilat[1]]], lon=PLOC[[ilon[1]]], LAT=PLOC[[ilat[1]]], LON=fmod(PLOC[[ilon[1]]], 360) )
        
        
        
        PG  = GLOB.XY(A$lat, A$lon , PROJ  )

        ##   plot(PG, asp=1)

        
        dx = (PG$x[2]-PG$x[1])
        dy = (PG$y[2]-PG$y[1])

        pct = 10/100

        newLL = XY.GLOB(PG$x[1]-0.1*dx, PG$y[1]-pct*dy, PROJ  )
        newUR = XY.GLOB(PG$x[2]+0.1*dx, PG$y[2]+pct*dy, PROJ  )

        
        newPLOC = list(lat=c(newLL$lat,newUR$lat )  , lon=c(newLL$lon,newUR$lon )   )


        ZZ2 = subsetTOPO(TOPO, newPLOC)
        
        
######   image(ZZ2)

        
        
        
      }
    
    
    d = dim(ZZ2$z)
    
    G = setplotmat(ZZ2$x,ZZ2$y)

######  image(ZZ2)

######    DOTOPOMAPI(TOPO=ETOPO5, worldmap=worldmap, shiftlon=0, ALOC=PLOC)

    
### read in the topo information from the GLOBE database
    
### first:  jtop = scan(file='jap.topo', list(lon=0, lat=0, z=0))

###   jlon = unique(ZZ2$x)
###   jlat = unique(ZZ2$y)




    GXY  = GLOB.XY(G$y, G$x , PROJ  )
    
    
###  xo = seq(from=range(GXY$x)[1], to=range(GXY$x)[2], length=npoints)
###   yo = seq(from=range(GXY$y)[1], to=range(GXY$y)[2], length=npoints)

##########   here, lets try to restrict the number of points for interpolation

    Gflag = GXY$x>=PG$x[1]  & GXY$x<=PG$x[2] & GXY$y>=PG$y[1] & GXY$y<=PG$y[2]

    gx = GXY$x[Gflag ]
    gy = GXY$y[Gflag ]
    gz=t(ZZ2$z)
    gz = gz[Gflag ]

    
    
    xo = seq(from=PG$x[1], to=PG$x[2], length=npoints)
    yo = seq(from=PG$y[1], to=PG$y[2], length=npoints)


    

##########   here we transpose the topo data to fit the next programs
    cat("Interpolating the topography....please wait, this can take time....", file="", sep="\n")
    
###  IZ = interp(x=GXY$x , y=GXY$y,  z=t(ZZ2$z)  , xo=xo, yo=yo, extrap=FALSE)

    ww = which(gx>=PG$x[1] & gx<=PG$x[2] & gy>=PG$y[1] & gy<=PG$y[2])
    
    
    IZ = interp(x=gx[ww] , y=gy[ww] ,  z=gz[ww]   , xo=xo, yo=yo, extrap=FALSE)



###    rect(PG$x[1], PG$y[1],     PG$x[2], PG$y[2]         )  
    ##  image(IZ, col=rainbow(100) )
    
    ##   H = setplotmat(IZ$x,IZ$y)
    ##  points(H$x[is.na(IZ$z)] , H$y[is.na(IZ$z)], pch=2)
    
    ##  jz = matrix(ZZ2$z, ncol=length(jx), nrow=length(jy), byrow=TRUE)



    if(PLOT==TRUE)
      {
        
        cat("Setting Colors....please wait....", file="", sep="\n")

       Cmat  = TOPOCOL(IZ, calcol)
        Dcol  = attr(Cmat, 'Dcol') 
        cat(".....plotting with persp....please wait....", file="", sep="\n")
        
        PMAT = persp(xo, yo, IZ$z, theta = 0, phi = 90, r=4000, col=Cmat[1:(Dcol[1]-1), 1:(Dcol[2]-1)] , scale = FALSE,
          ltheta = 120, lphi=30, shade = 0.75, border = NA, expand=0.001, box = FALSE )
        
      }
    else
      {
        PMAT = NA
      Cmat=NA
       Dcol=NA


      }
    
    
    invisible(list(PMAT=PMAT, xo=xo, yo=yo, IZ=IZ ,Cmat=Cmat, Dcol=Dcol))
  }

