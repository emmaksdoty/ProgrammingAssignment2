##These functions are writtin to fulfill the Coursera Data Science: R programming Week 3 Assignment

##This function creates a matrix object that can cache its inverse and defines the default mode as matrix type
makeCacheMatrix <- function(x = matrix()) {
## This sets the default of the variable as NULL and will be used to hold the value of the matrix inverse
    matinv <- NULL

    ##this defines the set function to assign a new matrix in a parent environment and if there is a new matrix, it assigns matinv to null
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  ##This assigns the get function to return the value of the matrix assignment
  get <-function() x
  ##This sets the value of matinv in the parent environment and the gets the value of matinv when called
  setinv <- function(solve) matinv <<- solve
  getinv <- function () matinv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
cacheSolve <- function(x = matrix, ...) {
  ##Return a matrix that is the inverse
  matinv <-x$getinv()
  if(!is.null(matinv)) {
    message("getting cached data")
    return(matinv)
  }
  data <- x$get()
  matinv <-solve(data, ...)
  x$setinv(matinv)
  matinv
}
