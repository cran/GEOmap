XSECwin<-function(SW, iseclab=1, xLAB="A" , labs=c("DONE","REFRESH", "PS" ), width=10, demo=FALSE)
  {
    if(missing(demo)) { demo = FALSE }
    if(missing(width)) { width=10 }
    if(missing(labs)) { labs=c("DONE","REFRESH", "PS" ) }
 if(missing(xLAB)) { xLAB="A" }
 if(missing(iseclab))  {  iseclab=1 }
    


    
    
#######  
    
##### source("/home/lees/XSECwin.R")

    ###  labs=c("DONE","REFRESH", "XSEC",  "CONT", "width", "PS" )
    
  ### XSECwin( SW , iseclab, xLAB , labs, demo=FALSE  )   
    #####  
    
    iseclab  = 0
    secmat = NULL
    ncol = 100
    TPALS = c("rainbow", "topo", "terrain")
    colabs = rep(1, length=length(labs))
    pchlabs = rep(0,length(labs))
    FUN = match.fun(TPALS[1])
    pal = FUN(ncol)

    ##########  xsec plotting  function:

  plot(SW$r , -SW$depth,  main=paste( iseclab, xLAB) , xlab="km", ylab="Depth", asp=1)
           

 a1 = range(SW$r, na.rm=TRUE)
            mtext(xLAB, side=3, at=a1[1])
            mtext(paste(sep="", xLAB, "'") , side=3, at=a1[2])
          
    if(demo==TRUE)  return(NULL)
           
    cdev = dev.cur()
 buttons = rowBUTTONS(labs, col=colabs, pch=pchlabs)
    NLABS = length(labs)
    NOLAB = NLABS +1000  ## some large number
    
    
    iloc = locator(1, type='p')
    zloc = iloc

    Nclick = length(iloc$x)
    if(is.null(zloc$x)) { return(NULL) }
    K =  whichbutt(zloc , buttons)
    sloc = zloc
    CONT.FLAG = FALSE
    XSEC.FLAG = FALSE
    PS.FLAG =  FALSE

 while(TRUE)
      {
        ############   button actions

        ###########   quit and break loop
        if(K[Nclick] == match("DONE", labs, nomatch = NOLAB))
          {


             buttons = rowBUTTONS(labs, col=rep(grey(.8), length(labs)), pch=rep("NULL", length(labs)))
            title("Return to Calling Program")
       
            break;
          }

        ###########   refresh the screen
        if(K[Nclick] == match("REFRESH", labs, nomatch = NOLAB))
          {
            zloc = list(x=NULL, y=NULL)
          }
 ###########   
        if(K[Nclick] == match("Next", labs, nomatch = NOLAB))
          {
            dev.set(dev.next())
            
            zloc = list(x=NULL, y=NULL)
          }


        if(K[Nclick] > 0)
          {
            
            
            if(PS.FLAG) {
              P = round(par('pin'), digits=2); 
              postscript(file="XMAPdemo.eps"  , width=P[1], height=P[2],
                         paper = "special", horizontal=FALSE, onefile=TRUE,print.it=FALSE)
            }

            
            
            plot(SW$r , -SW$depth,  main=paste( iseclab, xLAB) , xlab="km", ylab="Depth", asp=1)
            a1 = range(SW$r, ns.rm=TRUE)
            mtext(xLAB, side=3, at=a1[1])
            mtext(paste(sep="", xLAB, "'") , side=3, at=a1[2])
            
             buttons = rowBUTTONS(labs, col=colabs, pch=pchlabs)
            
            if(PS.FLAG) {
              dev.off();
              cat("the postscript file is in: RPMGdemo.eps", sep="\n")
              PS.FLAG =  FALSE
            }
            
            
          }
        else
          {
###  in case the plot was resized with asp=1, need to replot the buttons
            buttons = rowBUTTONS(labs, col=colabs, pch=pchlabs)
          }


        
        iloc = locator(1,type='p')
##### print(iloc)
        zloc  = list(x=c(zloc$x,iloc$x), y=c(zloc$y, iloc$y))
        Nclick = length(iloc$x)
        if(is.null(zloc$x)) { return(sloc) }
        K =  whichbutt(iloc , buttons)
##### print(K)   
      }
    

  }
