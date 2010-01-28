\name{ExplodeSymbols}
\Rdversion{1.1}
\alias{ExplodeSymbols}
\title{Explode symbols that overlap
}
\description{Interactive program for
  redistributing symbols for later plotting.  Used
  for Focal Mechanisms.
}
\usage{
ExplodeSymbols(XY, fsiz = 1, STARTXY = NULL)
}
\arguments{
  \item{XY}{list of x,y values
}
  \item{fsiz}{size of the symbol, as a percentage of the user coordinates
}
\item{STARTXY}{Starting positions.  This is used
  for multiple sessions where we want to pick up the previous locations.
}
}
\details{
  The program is interactive.  It starts by plotting the points as
  symbols.  A number of buttons are provided for exploding the points
  semi automatically.  To move each point click near its current point,
  then click at the destination followed by a click on the HAND button.
  several symbols can be moved at the same time.

You must click on the screen
and on the buttons to get this code working -
  the program will not work in batch mode or run as a script
You click in the active screen area and then press a button
on top (or bottom) - the button takes your clicks and does something
Here are some hints:


Buttons:

HAND: If you want to move only one symbol (focal mech) click near it and then click
where you want it to go.  Then click the HAND button
You may click several at once, but for each click oin a symbol
there has to be a click somewhere to relocate it.
(i.e. there must be an even number of clicks on the screen before hitting the
 HAND button)

SEL:

If you want to explode several symbols at once, first select them:
  click lower left, then upper right of rectangle enclosing the selection.
Once a selection is made it remains active until another selection is made
so you can keep changing the radius and center for different explosions
Then click CIRC.

RECT
Choose a rectangle (lower left and upper right), then click RECT for an explosion

RECT2
After selecting, choose a center and a distance.
symbols will be moved to a rectangular perimeter defined by the two points

CIRC
After selection, click once for the circle center, and a second time for the radius, then click CIRC


LINE
Not implemented yet


}
\value{list of new x,y values
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{rekt2line
}
\examples{

\dontrun{
F1 = list(x=rnorm(43), y=rnorm(43))
SMXY = ExplodeSymbols(F1, 0.03)



}

}
\keyword{misc}
