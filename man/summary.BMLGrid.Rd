\name{summary.BMLGrid}
\alias{summary.BMLGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	summary basic informations of BML grids
}
\description{
	Compute the dimension of the grids, number of cars that moved, that were blocked, 
	and the average velocity at each time step, the density of the plot,
	the color of the moved car
}
\usage{
summary.BMLGrid(g, gPlus1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
  	The initial grid at time t
}
  \item{gPlus1}{
  	The grid at time step t+1 (moved one step)
}
}
\details{
	The two grids passed onto the functions need to have the same dimesions,
	and the same amount of blue cars and red cars 
}
\value{
	The function returns a LIST:
	\item{row numbers}{row numbers of the initial grid}
	\item{column numbers}{column numbers of the initial grid}
	\item{number of blueCars}{Number of blueCars on the grid}
	\item{number of redCars}{Number of redCars on the grid}
	\item{Density}{Density of the initial grid}
	\item{number of blockedCars}{number of the cars that were blocked from t to t+1}
	\item{number of movedCars}{number of cars moved from t to t+1}
	\item{movedCar color}{the color of the cars moved from t to t+1}
	\item{velocity of movedCar}{velocity of the cars that moved from t to t+1}

}
\author{
	Yuki Liu
}
