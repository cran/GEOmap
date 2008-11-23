maps2GEOmap<-function(zz, wx=1, mapnam="temp" )
{
  if(missing(mapnam)) mapnam="temp"
  if(missing(wx)) wx = which(is.na(zz$x))

  
  #########   convert maps information from a maps data base to a GEOmap data
  Kx = vector()
  Ky = vector()
  Knum = vector()
  Knam = vector()
  Kindex = vector()
  Kstyle = vector()
  Kcode = vector()
  Kcol = vector()

  a1 = 1
  n  = length(wx)
  start = 0
  for(i in 1:n)
    {
      i1 = a1
      i2 = wx[i]-1
     ## print(c(i1, i2))
      el = length(i1:i2)
      Kx = c(Kx, zz$x[i1:i2])
      Ky = c(Ky, zz$y[i1:i2])

      a1 = wx[i]+1
      Knum[i] = el
      Kindex[i] =  start
      start = start + el
      
      Knam[i] = paste(mapnam, i, sep="_")
      Kstyle[i] = 2
      Kcode[i] =  'o'
      Kcol[i] =  'black'
      
    }

  i1 = a1
  i2 = length( zz$x)
  Kx = c(Kx, zz$x[i1:i2])
  Ky = c(Ky, zz$y[i1:i2])
  el = length(i1:i2)
  i = i+1
  Knum[i] = el
  Kindex[i] =  start
  start = start + el
  
  Knam[i] = paste(mapnam, i, sep="_")
  Kstyle[i] = 2
  Kcode[i] =  'o'
  Kcol[i] =  'black'

    NEWMAP = list(STROKES = list(nam =Knam , num =Knum , index =Kindex ,
                  col = Kcol, style = Kstyle, code = Kcode, LAT1 = NULL, LAT2 = NULL,
                  LON1 = NULL, LON2 = NULL), POINTS = list(lat = Ky,
                                               lon = Kx))

  NEWMAP =  boundGEOmap(NEWMAP )

  return(NEWMAP )
  

}

