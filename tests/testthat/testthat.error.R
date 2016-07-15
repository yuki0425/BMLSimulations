library(BMLSimulations)
context("Errors")

test_that("There should be an error", {
  expect_error(createBMLGrid( -1, 2, c(1,2)), 
               "Dimensions of the grid has to be positive") 
  expect_error(createBMLGrid( 10, 10, c(300,200)), 
               "Number of cars has to be positive and no more than the number of cells")
  
  expect_error(createBMLGrid( 10, 10, c(-10,200)), 
               "Number of cars has to be positive and no more than the number of cells")
  
})