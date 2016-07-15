\name{createBMLGrid}
\alias{createBMLGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Create an initial BML traffic flow grid
}
\description{
	Create a BML grid matrix with cars (user input number) at random position
}
\usage{
createBMLGrid(r = 100, c = 100, ncars = c(red = 1500, blue = 1500))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{
number of rows for the grid matrix
}
  \item{c}{
number of columns for the grid matrix
}
  \item{ncars}{
a vector with two elements: number of red cars and blue cars
}
}
\details{
The function creates a S3 class grid. See example
}
\value{
The function returns a S3 class grid.
The grid has blue and red cars at random positions 
}

\author{
Yuki Liu
}


\examples{

x = createBMLGrid(10, 10, c(20, 30))
class(x)
}

