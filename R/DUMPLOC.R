`DUMPLOC`<-function(zloc, dig=12)
{
    ##### I am not changing this to message just yet.
	if(missing(dig)) dig =12

    nam = deparse(substitute(zloc))
    message(paste(sep="", nam,"=list()"))
if(length(zloc$x)>0)
{
    message(paste(sep="", nam, "$x=c(", paste(format(zloc$x, digits=dig), collapse=","), ")") )
}
if(length(zloc$y)>0)
{
    message(paste(sep="", nam,"$y=c(", paste(format(zloc$y, digits=dig), collapse=","), ")") )
}

if(length(zloc$lat)>0)
{
    message(paste(sep="", nam,"$lat=c(", paste(format(zloc$lat, digits=dig), collapse=","), ")") )
}

if(length(zloc$lon)>0)
{
    message(paste(sep="", nam,"$lon=c(", paste(format(zloc$lon, digits=dig+1), collapse=","), ")") )
}

if(length(zloc$LON)>0)
{
    message(paste(sep="", nam,"$LON=c(", paste(format(zloc$LON, digits=dig+1), collapse=","), ")") )
}


if(length(zloc$LAT)>0)
{
    message(paste(sep="", nam,"$LAT=c(", paste(format(zloc$LAT, digits=dig), collapse=","), ")") )
}

}


