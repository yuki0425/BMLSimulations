\name{runBMLGrid}
\alias{runBMLGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Run BMLGrid matrix

}
\description{
Simulates BML traffic flows 
}
\usage{
runBMLGrid(g, numSteps = 10000, saveAll = FALSE, plotAll = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
  	The initial grid
}
  \item{numSteps}{
  	Number of total time steps 
}
  \item{saveAll}{
  	It is set \code{saveAll = FALSE} at default,
  	which allow the function only saves the final grid after the last step;
  	if \code{saveAll = TRUE},
  	the function saves the grid produced after every single step
}
  \item{plotAll}{
  	It is set to be \code{FALSE} at default that only plot out the grid for the final step,
  	if \code{TRUE}, the function plots every grid for every step
}
}
\details{
	Both \code{saveAll} and \code{plotAll} have to be true
	to plot out all the grids for every time step
}
\value{
	If \code{saveAll = TRUE}, return a list with \code{numSteps} of grids;
	otherwise return a final grid after the last time step
}

\author{
	Yuki Liu
}
