`selectPOLImap` <-
function(J, K )
  {
    #########  J = continent
    #########  K = type
  #####  if(missing(polybase)) { polybase = "/home/tequila/lees/RDATA/RMAPS"  }

    
    codenames = c("LASI", "LNAM", "LSAM", "LAFR", "LEUR")
    continents = c("asia","namer", "samer", "africa", "europe")
    kinds = c("bdy", "cil", "riv")

    

    Polifiles = c("africa.bdy",
      "africa.cil",
      "africa.riv",
      "asia.bdy",
      "asia.cil",
      "asia.riv",
      "europe.bdy",
      "europe.cil",
      "europe.riv",
      "namer.bdy",
      "namer.cil",
      "namer.pby",
      "namer.riv",
      "samer.bdy",
      "samer.cil",
      "samer.riv")
    
    w1 = grep(continents[J], Polifiles)
    w2 = grep( kinds[K] ,   Polifiles[w1])

    wp = w1[w2]

  ###  fil = paste(sep="/",polybase , Polifiles[wp])

 ###   print(paste(sep=' ', "reading in:", fil))

    
 ###   name2 = substr(Polifiles[wp], 1, nchar(Polifiles[wp])-6)
    
    ##   polit = getworldmap(fil)
    name2 =Polifiles[wp]

    if(!exists(name2))  data(list=name2)
    
   ###  load(fil)
 polit = get(name2)

    
    if(kinds[K]=="bdy")
      {
        polit$STROKES$col = rep("black", length(polit$STROKES$col))
      }

    if(kinds[K]=="cil")
      {
        polit$STROKES$col = rep("blue", length(polit$STROKES$col))
      }

    if(kinds[K]=="riv")
      {
        polit$STROKES$col = rep("blue", length(polit$STROKES$col))
      }


    


    
    return(polit)
    

  }

