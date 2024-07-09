`printGEOinfo` <-
function(MAP, kstroke)
  {


df = data.frame(id=kstroke, nam=MAP$STROKES$nam[kstroke],
  index=MAP$STROKES$index[kstroke],
  num=MAP$STROKES$num[kstroke],
  col=MAP$STROKES$col[kstroke],
  style=MAP$STROKES$style[kstroke],
   code=MAP$STROKES$code[kstroke]
  )

  
  Atemp = apply(df , 1, 'paste', collapse=' ')			
       message(paste(collapse=' ', names(df) )	)		
        message(paste(collapse='\n', Atemp))	

  }

