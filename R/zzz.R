.First.lib <- function(lib, pkg)
{
 ##  library.dynam("Rwave", pkg, lib)
  library.dynam("GEOmap", pkg, lib)
  
  cat("GEOmap is loaded\n")
}
