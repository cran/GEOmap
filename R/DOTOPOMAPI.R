`DOTOPOMAPI` <-
function(TOPO=TOPO, themap=NULL, shiftlon=0, ALOC=NULL, DOCONT=TRUE, DOIMG=TRUE,
                    PNTS=NULL, PCOL='red', PCH=1, PCEX=1, PS=FALSE, MAPPATH=NULL,
                     polybase = NULL,
                     USAmap= NULL,
                     usacity=NULL,
                     worldcity=NULL)
  {
###   TOPO = topographic DEM of the world
    ### worldmap = geographic map of world in GEO format structure
    ### shiftlon = rotate the longitude of map by this many degrees
    ###  DOCONT = plot contours on zoom
    ###  IMG plot  dem image on zoom
    ###  PNTS = list of points with lat and lon
    ###  PCOL=color for points, PCH=symbol for points, PCEX=size for points

    
    if(missing(TOPO))
      {
        require(geomapdata)
        data(ETOPO5)
        TOPO=ETOPO5
### 
###     fn5 = "/home/bourbon/BATHYM/ETOPO5.DOS"
###     TOPO = getbath(fn5)
      }
    if(missing(themap))
      {
        require(geomapdata)
        data(worldmap)
        
        themap=worldmap
###  fn = "/home/lees/Site/World/world.leesmap"
###    worldmap = getGEOmap(fn)
###    worldmap$POINTS$lon = fmod( worldmap$POINTS$lon, 360)
###   worldmap = boundGEOmap(worldmap)
      }
    if(missing(shiftlon)) { shiftlon=0 }
    if(missing(PNTS)) { PNTS=NULL }
    if(missing(PCOL)) { PCOL='red' }
    if(missing(PCH)) { PCH=1 }
    if(missing(PCEX)) { PCEX=1 }
    if(missing(DOCONT)) { DOCONT=TRUE }
    if(missing(DOIMG)) { DOIMG=TRUE }
    if(missing(PS)) { PS=FALSE }
    if(missing(ALOC)) { ALOC=NULL }
    if(missing(polybase)) { polybase = NULL}

    if(missing(USAmap)) { USAmap=NULL }

    if(missing(usacity)) {  usacity=NULL}
    if(missing(worldcity)) {  worldcity=NULL}

    require(akima)
    
    

    APALS = c("rainbow", "topo", "terrain", "heat", "tomo")
########  I removed GEOG until I can extract the Engdahl region stuff from Perl
###  labs = c("DONE", "Postscript", "BDY", "CIL", "RIV" , "IMG", "CONT", "GRID", "USA", "W-CIT" , "USA-CIT" , "GEOG"  )
    labs = c("DONE", "REFRESH","Postscript", "BDY", "CIL", "RIV" , "IMG", "CONT", "GRID", "USA", "W-CIT" , "USA-CIT"  )

    
    colabs = rep(1,length(labs))
    pchlabs = rep(4,length(labs))
    NLABS = length(labs)
    NOLAB = NLABS +1000

###  GRIDcol = gray(0.7)   default starting gridcol is NULL
    GRIDcol = NULL
    rivers = FALSE
    lakes = FALSE
    poliborders = FALSE
    LMAP=NULL

    
    pmap1 = NULL
    pmap2 = NULL
    pmap3 = NULL
   
    usalines = FALSE
    usacitylines = FALSE
    worldcitylines= FALSE
    
    if(is.null(ALOC))
      {
        plotworldmap(themap  ,shiftlon=shiftlon,  add=FALSE, col=2)
        
        if(!is.null(PNTS))
          {
            x = fmod(PNTS$lon-shiftlon, 360) 
            y = PNTS$lat
            points(x,y, col=PCOL, pch=PCH)
          }
        
        A = locworld(shiftlon)
      }
    else
      {
        A = ALOC
      }
    nlats=  A$lat
    
    DTOP = dim(TOPO)
    
    topolon = attr(TOPO, 'lon')
    topolat = attr(TOPO, 'lat')

    FLON = fmod(A$lon, 360)

    dlon = difflon(FLON[1], FLON[2])
    EL1 = FLON[1]+dlon$deg
    

   ####  wlats = which( topolat>nlats[1] & topolat<nlats[2] )
    wlats =  topolat>nlats[1] & topolat<nlats[2] 


####  if(diff(sign(A$lon))!=0)

    if(FLON[1]<360 & EL1>360)
      {
        ####   boundary crosses the 0-longitude and care must be taken

        ##  need to glue the two parts together

        mlon = fmod(mean(c(FLON[1], EL1)), 360)
        
        nlons = A$lon
        
        t1 = which(topolon>A$LON[1])
        t2 = which( topolon<A$LON[2])
        
        ax = c( topolon[t1]-360 , topolon[t2])
        ay = topolat[topolat>nlats[1]&topolat<nlats[2] ]
        PLON  = pretty(A$lon)
        PLON = c(min(A$lon), PLON[PLON>min(A$lon) & PLON<max(A$lon)], max(A$lon))

        
        mytop = TOPO[c(t1,t2)  , rev(topolat>nlats[1] & topolat<nlats[2] ) ]
        d = dim(mytop)
        
      }

    else
      {

        
        mlon = mean(A$LON)
        
        nlons = A$LON
        
        PLON  = pretty(A$LON)
        PLON = c(min(A$LON), PLON[PLON>min(A$LON) & PLON<max(A$LON)], max(A$LON))
        L = list(x=nlons, y = nlats)
        
        mytop = TOPO[topolon>L$x[1]&topolon<L$x[2]  , rev(topolat>L$y[1] & topolat<L$y[2] ) ]
        
        ax = topolon[topolon>L$x[1]&topolon<L$x[2]]
        ay = topolat[topolat>L$y[1]&topolat<L$y[2] ]
        d = dim(mytop)
        
      }
    
    
###   select a rectangle by clicking twice on map

     MAPLIM = c(A$LON[1], A$LAT[1], A$LON[2], A$LAT[2])

    PROJ = setPROJ(type="utm.sphr", LAT0=mean(A$lat) , LON0=mlon ,LAT1=NULL,  LAT2=NULL,
      LATS=NULL, LONS=NULL, DLAT=NULL, DLON=NULL, FE=0, FN=0)

    ## image(x=ax, y=ay,   z=mytop[, d[2]:1  ], col=topo.colors(100)  )
    ## contour(x=ax, y=ay,   z=mytop[, d[2]:1  ], add=TRUE)
    if(length(ax)<2)
      {
        print(paste(sep=' ', 'lenax=',length(ax), 'lenay=', length(ay)))
        print(A$LON)
        return("ERROR")
      }

    
    print(paste(sep=' ', 'lenax=',length(ax), 'lenay=', length(ay)))
    
   ## G2 = setplotmat(ax,ay)
##  G = meshgrid(ax, rev(ay) )  
##  G = meshgrid(ax, (ay) )  
    
    G = setplotmat(ax,ay)

    ## plot(G)

    B = GLOB.XY( G$y, G$x,  PROJ)

    ##  plot(B, asp=TRUE)

    xo = seq(from=range(B$x)[1], to=range(B$x)[2], length=500)
    yo = seq(from=range(B$y)[1], to=range(B$y)[2], length=500)


    IZ = interp(x=B$x , y=B$y,  z=t(mytop), xo=xo, yo=yo)

  ##  IZ = interp(x=B$x , y=B$y,  z=t(mytop[, rev(seq(from=1, to=d[2]))]), xo=xo, yo=yo)

    ##  image(x=xo, y=yo,   z=IZ$z, col=topo.colors(100), asp=TRUE , axes=FALSE, xlab="", ylab="" )
    ## contour(x=xo, y=yo,   z=IZ$z, add=TRUE  )
    ## points(P$x, P$y)
   
    UZ = IZ$z
    UZ[IZ$z>=0] = NA

    AZ = IZ$z
    AZ[IZ$z<=-.01] = NA


    zim = GLOB.XY( rep(min(G$y), length=50) ,seq(from=min(G$x), to=max(G$x), length=50),  PROJ )
    
    perim=zim


    zim = GLOB.XY( seq(from=min(G$y), to=max(G$y), length=50), rep(max(G$x), length=50) ,  PROJ )
    perim=list(x=c(perim$x, zim$x), y=c(perim$y, zim$y))

    zim = GLOB.XY( rep(max(G$y), length=50) ,seq(from=max(G$x), to=min(G$x), length=50),  PROJ )
    perim=list(x=c(perim$x, zim$x), y=c(perim$y, zim$y))

    zim = GLOB.XY( seq(from=max(G$y), to=min(G$y), length=50), rep(min(G$x), length=50),  PROJ  )
    perim=list(x=c(perim$x, zim$x), y=c(perim$y, zim$y))

    perim=list(x=c(perim$x, perim$x[1]), y=c(perim$y, perim$y[1]))
####  plot(perim$x, perim$y, type='l') ; lines( 
    PLAT =  pretty(A$LAT)

    PLAT = c(min(A$LAT),  PLAT[PLAT>min(A$LAT) & PLAT<max(A$LAT)],max(A$LAT)) 


    ###########  start the plotting  ###############

   BASICTOPOMAP(xo, yo , DOIMG, DOCONT, UZ, AZ, IZ, perim, PLAT, PLON,  PROJ=PROJ, pnts=PNTS, GRIDcol=GRIDcol)


    buttons = rowBUTTONS(labs, col=colabs, pch=pchlabs)
     #####  buttons = rowBUTTONS(labs, col=colabs, pch=pchlabs)
  
   #####    zloc = locworld(shiftlon, col='brown', n=0)

  ###### Nclick = length(zloc$x)
  ###### if(is.null(zloc$x)) { return(NULL) }
  ###### K = whichbutt(zloc ,buttons)

    iloc = ilocator(1, COL=rgb(1,0.8, 0.8), NUM=FALSE , YN=1, style=1)
    zloc = iloc
    Nclick = length(iloc$x)
    zenclick =  length(zloc$x)
    if(is.null(zloc$x))
      {
        print('nothing'); return(NULL)

      }
    K = whichbutt(iloc ,buttons)
    sloc = zloc
    
   while(TRUE)
     {

#############################################################
##############################################################
##############################################################       
       
       if(zenclick==0 & Nclick<1 )
         {
           buttons = rowBUTTONS(labs, col=rep(grey(.8), length(labs)), pch=rep("NULL", length(labs)))
           title("Return to Calling Program")
           
           break;
#### next
         }
#############################################################
##############################################################
##############################################################       

       
        if(zenclick>0 & Nclick<1 )
         {

           BASICTOPOMAP(xo, yo, DOIMG, DOCONT, UZ, AZ, IZ, perim, PLAT, PLON, PROJ=PROJ ,  pnts=PNTS, GRIDcol=GRIDcol)
           buttons = rowBUTTONS(labs, col=colabs, pch=pchlabs)
           zloc = list(x=NULL, y=NULL)
           zenclick =  length(zloc$x)
           Nclick = 1
        
#### next
         }

#############################################################
##############################################################
##############################################################       
 
       
       if(K[Nclick] == match("DONE", labs, nomatch = NOLAB))
         {

           buttons = rowBUTTONS(labs, col=rep(grey(.8), length(labs)), pch=rep("NULL", length(labs)))
           title("Return to Calling Program")


           break;
         }



#############################################################
##############################################################
##############################################################       
      


     if(K[Nclick] == match("REFRESH", labs, nomatch = NOLAB))
         {

           BASICTOPOMAP(xo, yo, DOIMG, DOCONT, UZ, AZ, IZ, perim, PLAT, PLON, PROJ=PROJ ,  pnts=PNTS, GRIDcol=GRIDcol)
           buttons = rowBUTTONS(labs, col=colabs, pch=pchlabs)
           zloc = list(x=NULL, y=NULL) 
           
         }


#############################################################
##############################################################
##############################################################       

       
          
        if(K[Nclick] == match("Postscript", labs, nomatch = NOLAB))
          {
            print(paste(sep=' ' ,"Start postscript file plotwlet"))
           jdev = dev.cur()
           ##  plfname = local.file("jmap","eps")

            jpostscript("DOTOPO")

            BASICTOPOMAP(xo, yo, DOIMG, DOCONT, UZ, AZ, IZ, perim, PLAT, PLON, PROJ=PROJ ,  pnts=PNTS, GRIDcol=GRIDcol)
            
            if(poliborders==TRUE)
              {
                plotGEOmapXY(pmap1,  PROJ=PROJ  , LIM=MAPLIM, add=TRUE);
                
              }
            
            if(lakes==TRUE)
              {
                plotGEOmapXY(pmap2,  PROJ=PROJ  , LIM=MAPLIM, add=TRUE);
                
              }

            if(rivers==TRUE)
              {
                plotGEOmapXY(pmap3,  PROJ=PROJ   , LIM=MAPLIM, add=TRUE);
                
              }
            dev.off()
            dev.set(jdev)
            
             zloc = list(x=NULL, y=NULL)
          zenclick = 0


            

          }
#############################################################
##############################################################
##############################################################       

       if(K[Nclick] == match("CONT", labs, nomatch = NOLAB))
          {
            DOCONT = !DOCONT
            BASICTOPOMAP(xo, yo, DOIMG, DOCONT, UZ, AZ, IZ, perim, PLAT, PLON, PROJ=PROJ, pnts=PNTS, GRIDcol=GRIDcol)
             zloc = list(x=NULL, y=NULL)
          zenclick = 0


  
          }
#############################################################
##############################################################
##############################################################       

       if(K[Nclick] == match("IMG", labs, nomatch = NOLAB))
          {
            DOIMG = !DOIMG
            BASICTOPOMAP(xo, yo, DOIMG, DOCONT, UZ, AZ, IZ, perim, PLAT, PLON, PROJ=PROJ, pnts=PNTS, GRIDcol=GRIDcol)
            
            zloc = list(x=NULL, y=NULL)
            zenclick = 0


          }
#############################################################
##############################################################
##############################################################       
       
       if(K[Nclick] == match("GRID", labs, nomatch = NOLAB))
          {
            if(is.null(GRIDcol) ){    GRIDcol = gray(0.7) } else {GRIDcol =NULL}
            BASICTOPOMAP(xo, yo, DOIMG, DOCONT, UZ, AZ, IZ, perim, PLAT, PLON, PROJ=PROJ, pnts=PNTS, GRIDcol=GRIDcol)
             zloc = list(x=NULL, y=NULL)
          zenclick = 0


  
          }
       #############################################################
##############################################################
##############################################################       

       if(K[Nclick] == match("BDY", labs, nomatch = NOLAB))
          {
            if(is.null(LMAP)) { LMAP = SETPOLIMAP() }
            if(is.null(pmap1))
              {
                P = list(lat=mean(ay), lon=mean(ax))
                J = LOCPOLIMAP(P, LMAP)
                
                pmap1 = selectPOLImap(which(J==1), 1)
              }
            
          poliborders=!poliborders
            
            BASICTOPOMAP(xo, yo, DOIMG, DOCONT, UZ, AZ, IZ, perim, PLAT, PLON, PROJ=PROJ, pnts=PNTS, GRIDcol=GRIDcol)
             zloc = list(x=NULL, y=NULL)
          zenclick = 0


             
  
          }
 #############################################################
##############################################################
##############################################################       
      
       if(K[Nclick] == match("CIL", labs, nomatch = NOLAB))
          {
            if(is.null(LMAP)) { LMAP = SETPOLIMAP() }
            if(is.null(pmap2))
              {
                P = list(lat=mean(ay), lon=mean(ax))
                J = LOCPOLIMAP(P, LMAP)
                
                pmap2 = selectPOLImap(which(J==1), 2)
                
              }
            
          lakes=!lakes
         
            BASICTOPOMAP(xo, yo, DOIMG, DOCONT, UZ, AZ, IZ, perim, PLAT, PLON, PROJ=PROJ, pnts=PNTS, GRIDcol=GRIDcol)
             zloc = list(x=NULL, y=NULL)
          zenclick = 0


  
          }
#############################################################
##############################################################
##############################################################       
       
       if(K[Nclick] == match("RIV", labs, nomatch = NOLAB))
          {
            if(is.null(LMAP)) { LMAP = SETPOLIMAP() }
            if(is.null(pmap3))
              {
                P = list(lat=mean(ay), lon=mean(ax))
                J = LOCPOLIMAP(P, LMAP)
                
                pmap3 = selectPOLImap(which(J==1), 3)
              }
            
          rivers=!rivers
         
            BASICTOPOMAP(xo, yo, DOIMG, DOCONT, UZ, AZ, IZ, perim, PLAT, PLON, PROJ=PROJ, pnts=PNTS, GRIDcol=GRIDcol)
             zloc = list(x=NULL, y=NULL)
          zenclick = 0


  
          }
#############################################################
##############################################################
##############################################################       

       if(K[Nclick] == match("USA", labs, nomatch = NOLAB))
          {
            
            if(is.null(USAmap))
              {
               data(USAmap)
                  }
            
          usalines=!usalines
         
            BASICTOPOMAP(xo, yo, DOIMG, DOCONT, UZ, AZ, IZ, perim, PLAT, PLON, PROJ=PROJ, pnts=PNTS, GRIDcol=GRIDcol)
             zloc = list(x=NULL, y=NULL)
          zenclick = 0


  
          }
#############################################################
##############################################################
##############################################################       

      if(K[Nclick] == match("USA-CIT", labs, nomatch = NOLAB))
          {
            
            if(is.null(usacity))
              {
               
            ######     usacity=scan(file=usacity, list(name='', lat=0, lon=0, p=0))
                data(usacity)
              }
                usacity$lon = fmod(usacity$lon, 360)
                XY =  GLOB.XY(usacity$lat, usacity$lon , PROJ  )
                usacity$x = XY$x
                usacity$y = XY$y
                usacity$LLflag = usacity$lat>min(A$LAT)&usacity$lat<max(A$LAT)&usacity$lon>min(A$LON)&usacity$lon<max(A$LON)
                usacity$pflag = usacity$p>50000
                usacity$flag = usacity$pflag & usacity$LLflag
               print(paste(sep=' ', "usacity", length(usacity$x),  length(usacity$x[usacity$flag ])))
              
            
          usacitylines=!usacitylines
         if(length(usacity$x[usacity$flag ])< 1 )  usacitylines =FALSE
            BASICTOPOMAP(xo, yo, DOIMG, DOCONT, UZ, AZ, IZ, perim, PLAT, PLON, PROJ=PROJ, pnts=PNTS, GRIDcol=GRIDcol)
            zloc = list(x=NULL, y=NULL)
          zenclick = 0


          }
#############################################################
##############################################################
##############################################################       

      if(K[Nclick] == match("W-CIT", labs, nomatch = NOLAB))
          {
            
            if(is.null(worldcity))
              {
               
            ######     worldcity=scan(file=worldcity, list(name='', lat=0, lon=0, p=0))
                worldcity=data(worldcity)
              }
                worldcity$lon = fmod(worldcity$lon, 360)
                XY =  GLOB.XY(worldcity$lat, worldcity$lon , PROJ  )
                worldcity$x = XY$x
                worldcity$y = XY$y
                worldcity$LLflag = worldcity$lat>min(A$LAT)&worldcity$lat<max(A$LAT)&worldcity$lon>min(A$LON)&worldcity$lon<max(A$LON)
              ######  worldcity$pflag = worldcity$p>5000
                worldcity$flag = worldcity$LLflag
               print(paste(sep=' ', "worldcity", length(worldcity$x),  "In target=", length(worldcity$x[worldcity$flag ])))
              
            
          worldcitylines=!worldcitylines
         if(length(worldcity$x[worldcity$flag ])< 1 ) worldcitylines =FALSE
            BASICTOPOMAP(xo, yo, DOIMG, DOCONT, UZ, AZ, IZ, perim, PLAT, PLON, PROJ=PROJ, pnts=PNTS, GRIDcol=GRIDcol)
            zloc = list(x=NULL, y=NULL)
          zenclick = 0


          }


       
##############################################################  this is defunct for now....
##############################################################  Engdahl regions
  ######     if(K[Nclick] == match("GEOG", labs, nomatch = NOLAB))
  ######       {
           
   ######        L = XY.GLOB(zloc$x[zenclick-1 ], zloc$y[ zenclick-1], PROJ )
   ######        lon = fmod(L$x[1], 360)

   ######        data(feregion)
         #######  place = system(paste(sep=' ', "/home/lees/Progs/Perl/feregion.prl" ,L$lon[1] , L$lat[1]), intern=TRUE)
         
  ######         text(zloc$x[zenclick-1 ], zloc$y[ zenclick-1], labels=place, pos=4, cex=1)
 ######          print(place)
 ######           zloc = list(x=NULL, y=NULL)
######          zenclick = 0  
   ######      }
#############################################################
##############################################################
##############################################################       

       
       if(poliborders==TRUE)
         {
            plotGEOmapXY(pmap1, PROJ=PROJ  , LIM=MAPLIM, add=TRUE);
         }
#############################################################
##############################################################
##############################################################       
       

      if(lakes==TRUE)
         {
            plotGEOmapXY(pmap2, PROJ=PROJ  , LIM=MAPLIM, add=TRUE);
         }
#############################################################
##############################################################
##############################################################       
       

      if(rivers==TRUE)
         {
            plotGEOmapXY(pmap3, PROJ=PROJ  , LIM=MAPLIM, add=TRUE);
         }
#############################################################
##############################################################
##############################################################       

      if(usalines==TRUE)
         {
            plotGEOmapXY(USAmap , PROJ=PROJ , LIM=MAPLIM, add=TRUE);
         }
#############################################################
##############################################################
##############################################################       

      if(worldcitylines==TRUE)
         {
           if(length(worldcity$x[worldcity$flag])>0)
             {
            points(worldcity$x[worldcity$flag], worldcity$y[worldcity$flag])
            text(worldcity$x[worldcity$flag], worldcity$y[worldcity$flag],
                 labels = worldcity$name[worldcity$flag], pos=4, cex=.5)
          }
             
         }


       
#############################################################
##############################################################
##############################################################       

      if(usacitylines==TRUE)
         {
           if(length(usacity$x[usacity$flag])>0)
             {
               points(usacity$x[usacity$flag], usacity$y[usacity$flag])
               text(usacity$x[usacity$flag], usacity$y[usacity$flag],
                    labels = usacity$name[usacity$flag], pos=4, cex=.5)
             }
           
         }
#############################################################
##############################################################
##############################################################       

        buttons = rowBUTTONS(labs, col=colabs, pch=pchlabs)
  

       iloc = ilocator(1, COL=rgb(1,0.8, 0.8), NUM=FALSE , YN=1, style=1)
       Nclick = length(iloc$x)
       
       if(Nclick>0)
         {
           zloc  = list(x=c(zloc$x,iloc$x), y=c(zloc$y, iloc$y))
           zenclick = length(zloc$x)
           K =  whichbutt(iloc ,buttons)
         }
       else
         {
           Nclick = 0
           K = 0
         }
#############################################################
##############################################################
##############################################################       

       
     }

#############################################################
##############################################################
##############################################################       
    
   print("Done")
   
    
  }

