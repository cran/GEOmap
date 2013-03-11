`geoLEGEND` <-
function(names, shades, zx, zy, nx, ny)
{

u = par("usr")
pin = par("pin")

dx  = zx*(u[2]-u[1])/pin[1]
dy = zy* (u[4]-u[3])/pin[2]

N = length(names)

for(i in 1:N)
{

ii = i
ixy = RPMG::itoxyz(i, nx, ny,1)

##print(paste(sep=" ", i, ixy$ix, ixy$iy))

x1 = u[1]+(ixy$ix-1)*dx
x2 = x1+dx
y1 = u[3] -(ixy$iy+1)*dy
y2 = y1+dy

rect(x1,y1,x2,y2, col=shades[ii], xpd=TRUE)
text(mean(c(x1,x2)), mean(c(y1,y2)), labels=names[ii], xpd=TRUE, cex=.5)

}
}

