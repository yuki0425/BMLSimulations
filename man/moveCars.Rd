\name{moveCars}
\alias{moveCars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Move Cars
}
\description{
Move one type of car for one step
}
\usage{
moveCars(g, color = "red")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
Grid matrix with "red", "blue", and ""
}
  \item{color}{
Color of the cars that want to be moved
}
}
\details{
If color is "blue", then the blue cars will be moved upward on the plot
If color is "red", then the red cars will be moved rightward on the plot
When a car gets to the edge of the grid, it "wraps" around, i.e., 
when a blue car gets to the top row, the next time it moves goes to the bottom row of the same column.
A car cannot move to cell if that cell is already occupied by another car of any color.
}
\value{
The function returns a grid
}

\author{
	Yuki Liu
}
