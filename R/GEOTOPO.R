`GEOTOPO` <-
function(TOPO, PLOC, PROJ, calcol=NULL, npoints=500)
  {


    if(missing(calcol)) {  calcol = settopocol() }
    if(missing(npoints)) { npoints=500  }


     if(is.null(calcol)) {  calcol = settopocol() }

    
    ZZ2 = subsetTOPO(TOPO, PLOC)
    d = dim(ZZ2$z)
    
    G = setplotmat(ZZ2$x,ZZ2$y)

    ######  image(ZZ2)

######    DOTOPOMAPI(TOPO=ETOPO5, worldmap=worldmap, shiftlon=0, ALOC=PLOC)

    
### read in the topo information from the GLOBE database
    
### first:  jtop = scan(file='jap.topo', list(lon=0, lat=0, z=0))

    jlon = unique(ZZ2$x)
    jlat = unique(ZZ2$y)




    GXY  = GLOB.XY(G$y, G$x , PROJ  )
    
    
    xo = seq(from=range(GXY$x)[1], to=range(GXY$x)[2], length=npoints)
    yo = seq(from=range(GXY$y)[1], to=range(GXY$y)[2], length=npoints)

    ##########   here we transpose the topo data to fit the next programs
    IZ = interp(x=GXY$x , y=GXY$y,  z=t(ZZ2$z)  , xo=xo, yo=yo, extrap=FALSE)


    ##  image(IZ, col=rainbow(100) )
    
    ##   H = setplotmat(IZ$x,IZ$y)
    ##  points(H$x[is.na(IZ$z)] , H$y[is.na(IZ$z)], pch=2)
    
    ##  jz = matrix(ZZ2$z, ncol=length(jx), nrow=length(jy), byrow=TRUE)
    

    UZ = IZ$z
    UZ[IZ$z>= .001 ] = NA

    AZ = IZ$z
    AZ[IZ$z<=-.001] = NA

    blues = shade.col(100, acol=as.vector(col2rgb("darkblue")/255)   , bcol= as.vector(col2rgb("paleturquoise")/255))

    ##  image(x=xo, y=yo,   z=UZ, col=blues, asp=TRUE , axes=FALSE, xlab="", ylab="" )

    ##  image(x=xo, y=yo,   z=AZ, col=topo.colors(100), asp=TRUE , axes=FALSE, xlab="", ylab="", add=TRUE )

    ##  plotGEOmapXY(japmap, PROJ=PROJ,CZ LIM=c(A$LON[1], A$LAT[1],A$LON[2], A$LAT[2] ) , add=TRUE)


    
    CZ = AZ
   ########  TZ[TZ<0] = NA

    dz = dim(AZ)
    
    coltab = cbind(calcol$r1, calcol$g1, calcol$b1,calcol$r2, calcol$g2, calcol$b2)
    
    coltab = rbind(coltab, coltab[length(calcol$r1),])
    
    rng = range(AZ[!is.na(AZ)])
    ncol = 100
    levs = seq(from=rng[1], to=rng[2], length=100)
    
    
    brs = c(calcol$z1[1], calcol$z2,  calcol$z2[length(calcol$z2)]+10000)
    
    Cs = CZ

    naflag = which(is.na(Cs))
    
    fs = findInterval(Cs, brs)
    df = diff(brs)
    
    coldis = (Cs - brs[fs])/df[fs]
    
    newcol = list(r= round(coltab[fs,1]+coldis*(coltab[fs,4]-coltab[fs,1]  )),
      g = round(coltab[fs,2]+coldis*(coltab[fs,5]-coltab[fs,2]  )),
      b = round(coltab[fs,3]+coldis*(coltab[fs,6]-coltab[fs,3]  )))

    newcol$r[naflag] = 0
    newcol$g[naflag] = 0
    newcol$b[naflag] = 0

    
    Collist = rgb(newcol$r/255, newcol$g/255, newcol$b/255)

    
#### bluesrgb = col2rgb(blues)

####    Collist[IZ$z<0] = blues
    
####   Mollist = matrix(data=Collist, ncol=dz[2], nrow=dz[1])
    
####    PMAT = persp(jx, jy , TZ, theta = 0, phi = 90, r=4000, col=Mollist[1:479, 1:359] , scale = FALSE,
####      ltheta = 120, lphi=60, shade = 0.75, border = NA, expand=0.001, box = FALSE )


   if( all(is.na(UZ)) )
     {
       C2 = Collist
     }
   else
     {
       
       bluesrgb = col2rgb(blues)

       rngU = range(UZ[!is.na(UZ)])
       
       ncol = 100
       levs = seq(from=rngU[1], to=rngU[2], length=100)
       
       
       brs = levs
       Cs = UZ[!is.na(UZ)]
       fs = findInterval(Cs, brs)
       
       
####   df = diff(brs)
    
####  coldis = (Cs - brs[fs])/df[fs]
       
       
       Anewcol = list(r=bluesrgb[1,fs],
         g =bluesrgb[2,fs] ,
         b =bluesrgb[3,fs] )
       
       
       UCollist = rgb(Anewcol$r/255, Anewcol$g/255, Anewcol$b/255)
       
       C2 = Collist
       C2[!is.na(UZ)] = UCollist
       
     }
    
    Mollist = matrix(data=C2, ncol=dz[2], nrow=dz[1])
    
    dMOL = dim(Mollist)       
    
    
    PMAT = persp(xo, yo, IZ$z, theta = 0, phi = 90, r=4000, col=Mollist[1:(dMOL[1]-1), 1:(dMOL[2]-1)] , scale = FALSE,
      ltheta = 120, lphi=30, shade = 0.75, border = NA, expand=0.001, box = FALSE )
    
  
    
    
    invisible(list(PMAT=PMAT, xo=xo, yo=yo, IZ=IZ , Mollist=Mollist, dMOL=dMOL))
  }

