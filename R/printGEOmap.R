`printGEOmap` <-
function(G)
  {

DF =  data.frame(G$STROKES) 

Atemp = apply(DF , 1, 'paste', collapse=' ')			
       message(paste(collapse=' ', names(DF) )	)		
        message(paste(collapse='\n', Atemp))		


  }

