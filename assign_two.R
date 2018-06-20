## PROGRAMMING ASSIGNMENT TWO

## HELPER FUNCTIONS

## MAKE VECTOR HELPER FUNCTION
## CLASS THAT CREATES VECTOR STRUCTURE WITH IT'S HELPING FUNCTIONS
## HELPING FUNCTION
## SET
## GET

makeVector <- function(x = numeric()) {
  mean <- NULL
  set <- function(y) {
    x <<- y
    mean <<- NULL
  }
  
  get <- function() x
  
  set_mean <- function(mean) mean <<- mean
  get_mean <- function() mean
  
  list(set = set, get = get, set_mean = set_mean, get_mean = get_mean)
}

cachemean <- function(x, ...) {
  mean <- x$get_mean()
  if(! is.null(mean) ) {
    message("returning cached mean")
    return(mean)
  }
  
  vec <- x$get()
  mean <- mean(data, ...)
  x$set_mean(mean)
  mean
}

## ASSIGNMENT FUNCTIONS

## CREATES DATA STRUCTURE FOR MATRIX AND IT'S INVERSE
## get: RETURN MATRIX
## set: SET NEW MATRIX DATA
## get_inverse: GETS MATRIX INVERSE
## set_inverse: SET NEW MATRIX INVERSE

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  get <- function() x
  get_inverse <- function() inverse
  
  set <- function(y) {
    x <<- y
    inverse <- NULL
  }
  set_inverse <- function(solve_matrix){
    inverse <<- solve_matrix
  }
  
  list(get = get, set = set, get_inverse = get_inverse, set_inverse = set_inverse)
}

## SOLVES MATRIX INVERSE AND SAVE IT IN DATA STRUCTURE
## PARAMS 
## X: MATRIX TO BE SOLVED

cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  if(!is.null(inverse)){
    message("returning cached inversion")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix)
  x$set_inverse(inverse)
  inverse
}