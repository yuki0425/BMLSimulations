##=================================
# CreatBMLGrid function creates a basic two dimension grid/matrix
# User input the following non-negative intergers:
# r: row number, c: column number
# c(red, blue): red car and blue car number and they dont have to be equal
# Assign S3 class to the grid and return the grid
##=================================
createBMLGrid = 
  function(r = 100, c = 100, ncars = c(red = 1500, blue = 1500) )
  {
   if (r>0 & c>0) {
     if (ncars[1] >= 0 && ncars[2] >= 0 && (ncars[1]+ncars[2])<= r*c) {
       dims = c(r, c)
       grid = matrix("", r, c)
       pos = sample(1:prod(dims), sum(ncars))
       grid[pos] = sample(rep(c("red", "blue"), ncars))
       # S3 class
       class(grid) = append("BMLGrid", class(grid))
       grid
     } else {
       stop ("Number of cars has to be positive and no more than the number of cells")
     }
   } else stop ("Dimensions of the grid has to be positive")
    
  }

# Plot the S3 class grid with red block and blue block represent red cars and blue cars
plot.BMLGrid = 
  function(x,...)
  {
    z = matrix(match(x, c("", "red", "blue")), nrow(x), ncol(x))
    image(t(z), col = c("white", "red", "blue"), axes = FALSE, xlab = "", ylab = "", ...)
    box()
  }


# Move Cars
# Since the grid is basically a big matrix, 
# we can get the location (coordinates) of the current red/blue car

# Find out the neighbourhood situation
# Then determine wheter the car can move

getCarLocations = 
  function(g)  # g is the grid we pass to the function
  { 
    rowIndex = row(g)[g!=""] # where it is not blank
    colIndex = col(g)[g!=""]
    # put all the index in to dataframe
    # Matrix subsetting thanks to Duncan and Piazza
    data.frame(i = rowIndex, j = colIndex, colors = g[cbind(rowIndex, colIndex)])
  }

## Method 1 (faster)
moveCars = 
  function(g, color = "red")  
    # g: the grid we want to pass to the function
    # color: color of the car moves from t to t+1
  {
    RedBlue = getCarLocations(g)
    # find the location of the colored car
    full = which(RedBlue$colors == color)
    rowsIndex = RedBlue[full, 1]
    colsIndex = RedBlue[full, 2]
    
    # If ask to move the red car, then move it to the right
    if(color == "red") {
      # Stay the same row
      nextRowIndex = rowsIndex
      nextColIndex = colsIndex + 1L
      # Wrap around/reset if move out of the grid
      nextColIndex[nextColIndex > ncol(g)] = 1L
    } else {
      # if the parameter specify "blue" 
      # Stat the same colum but move up one row
      nextRowIndex = rowsIndex + 1L
      nextColIndex = colsIndex
      nextRowIndex[nextRowIndex > ncol(g)] = 1L
    }
    # Check whether the next location for red/blue cars is actually available
    # subset the grid matrix using the next location matrix, yay piazza!
    nextLoca = cbind(nextRowIndex, nextColIndex)
    move = g[nextLoca] == "" 
    g[nextLoca[move,,drop = FALSE]] = color #only those ones count
    # The ones that moved should leave a blank space
    g[cbind(rowsIndex, colsIndex)[move,, drop = FALSE]] = ""
    
    g    
  }

# run Blue car then Red car 

# a function to compute the number of cars that moved, 
# that were blocked, and the average velocity
summary.BMLGrid= 
  function(g,gPlus1)
  {
    if(nrow(g)!=nrow(gPlus1)|ncol(g)!=ncol(gPlus1)) {
      stop ("Error: Two arguments need to have the same dimensions")
    } else {
      rows = nrow(g)
      cols = ncol(g)
      blueCars = sum(g=="blue")
      redCars = sum(g=="red")
      if (blueCars==sum(gPlus1=="blue") && redCars==sum(gPlus1=="red")) {
        locaT = getCarLocations(g)
        locaPlus1 = getCarLocations(gPlus1)
        total = rbind(locaT, locaPlus1)
        blockedCars = sum(duplicated(total))
        movedCars = blueCars + redCars - blockedCars
        density = (blueCars + redCars)/(rows*cols)
        # locations with the cars that moved 
        # (including index of origin and index after moved)
        moved = total[!(duplicated(total) | duplicated(total, fromLast = TRUE)), ]
        # determine the color of the moved car
        colour = unique(moved$colors)
        if (length(colour) == 0) {
          velocity = 0
          colour = "No car moves"
        } else {
          if (colour == "red") {
            velocity  = movedCars/redCars
          } else velocity = movedCars/blueCars
        }
        
        
        summaryMoves = list(rows, cols, blueCars, redCars, density, 
                            blockedCars, movedCars, 
                            colour, velocity)
        names(summaryMoves) = c("row numbers", "column numbers", 
                                "number of blueCars", 
                                "number of redCars", "Density", 
                                "number of blockedCars", 
                                "number of movedCars", "movedCar color", 
                                "velocity of movedCar")
        
        summaryMoves
      } else stop ("Error: Two grids need have the same number of cars")
      
    }
    
  }


# runBMLGrid() allow user to input the steps of the moving car
runBMLGrid = 
  function(g, numSteps = 10000, saveAll = FALSE, plotAll = FALSE)
    # g: the initial grid before any car moves
    # numSteps: a positive integer that specifiy the number of time steps cars move
    # blue cars move at time periods t = 1, 3, 5.. (Odd times)
    # red cars move at time periods t = 2, 4, 6.. (Even times)
    # saveALL gives user a choice to save grids for every single step
  {
    density = summary.BMLGrid(g, moveCars(g))$Density
    if (saveAll == TRUE) {
      # Save all the grids OMG!!
      AllGrids = lapply(rep(c("blue", "red"), numSteps%/%2), 
                        function(i){g <<- moveCars(g, i)})
      if (numSteps %% 2 ==0) {
        FinalGrid = AllGrids
      } else {
        # last one shall be an odd number and will be blue car to move
        lastGrid = moveCars(tail(AllGrids, 1), "blue")
        FinalGrid = lastGrid
        FinalGrid[[numSteps]] = lastGrid
      }
    } else {
      for (i in 1:(numSteps%/%2)) {
        g = moveCars(g, "blue")
        g = moveCars(g, "red")     
      }
      if (numSteps %% 2 ==0) {
        FinalGrid = g
      } else {
        # last one shall be an odd number and will be blue car to move
        FinalGrid = moveCars(g, "blue")
      }
    }
  if (saveAll && plotAll) {
    for (i in 1:numSteps) {
      plot.BMLGrid(FinalGrid[[i]], main = paste0("Step", i,
                                                 "; Density = ", density))
    }
  } else if (saveAll && !plotAll) {
    plot.BMLGrid(FinalGrid[[numSteps]], main = paste0("Step", numSteps,
                                                      "; Density = ", density))
  } else plot.BMLGrid(FinalGrid, main = paste0("Step", numSteps,
                                               "; Density = ", density))
  
  FinalGrid
}



