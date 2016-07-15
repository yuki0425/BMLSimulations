\name{BMLSimulations-package}
\alias{BMLSimulations-package}
\alias{BMLSimulations}
\docType{package}
\title{
	Simulates a two-dimensional Biham-Middleton-Levine traffic flow 
}
\description{
   The package creates a grid by placing the amount of user specified cars at random positions.
   The blue cars move at time periods t = 1, 3, 5, ... and the red cars move at t = 2, 4, 6, ...
   The blue cars move vertically upward and the reds move horizontally rightwards. 
   Cars wrap around when they get to the edge of the grid. 
   A car cannot move to cell if that cells is already occupied by another car of any color. 
}
\details{
\tabular{ll}{
Package: \tab BMLSimulations\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-04-30\cr
License: \tab MIT \cr
}
}
\author{
	Yuki Liu
	Maintainer: Yuki Liu <yukliu@ucdavis.edu>
}

\keyword{ package }
