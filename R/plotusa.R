`plotusa` <- function()
{
  require(geomapdata)
  
  data(USAmap)
  
  USALL=list()
  USALL$lat=c(24.72853,49.62741)
  USALL$lon=c(229.29389,296.41803)
  PROJ = setPROJ(type = 2, LAT0 =mean(USALL$lat), LON0 = mean(USALL$lon) )

  plotGEOmapXY(USAmap, LIM= c(USALL$lon[1], USALL$lat[1], USALL$lon[2], USALL$lat[2]    )  , 
               PROJ=PROJ, add=FALSE, shiftlon=0, axes=FALSE, ann=FALSE)
  
  invisible(list(PROJ=PROJ, USALL=USALL, LIM= c(USALL$lon[1], USALL$lat[1], USALL$lon[2], USALL$lat[2]    )))
  
}
