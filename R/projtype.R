`projtype` <-
function(proj=list())
  {
      if(missing(proj)) { proj = NULL }
      
    message("Projection Types")
    message("0 = None")
    message("1 = merc.sphr")
    message("2 = utm.sphr")
    message("3 = lambert.cc")
    message("4 = stereo.sphr")
    message("5 = utm.elps")
    message("6 = equid.cyl")
    
    message("99 = old crosson projection")

    if(!is.null(proj))
      {
        message(paste(sep=" ", "Current:", proj$type, proj$name))
      }
    

  }

