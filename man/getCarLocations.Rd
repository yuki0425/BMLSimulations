\name{getCarLocations}
\alias{getCarLocations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get Car Location Index
}
\description{
Get the row index and column index for the red cars and blue cars on the grid

}
\usage{
getCarLocations(g)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
S3 class Grid with "red" and "blue" and "" cells
}
}
\details{
	function finds the index of the "red" and "blue" on the grid
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
The function returns a dataframe with the row, column index and the color of the cars
}

\author{
	Yuki Liu
}




