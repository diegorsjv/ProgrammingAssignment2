## Developed by Diego J.V, based on the "Caching the Mean of a Vector" course example code.
## This R code corresponds to my submition for the second Programming Assignment.
## The overall purpose of the code is to create a way to cache the inverse of a matrix.
## In doing so, computational cost may be reduced for the matrix inversion operation. 

## This makeCacheMatrix function creates a special kind of "matrix" which is in
## fact an object that allows us to set and get values for our matrix and it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverseResult) inverseMatrix<<-inverseResult
  getInverseMatrix <- function() inverseMatrix
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## This function is the one that computes the inverse matrix and caches the result
## using the special matrix created in the previous makeCacheMatrix function. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverseMatrix()
  if(!is.null(inverseMatrix)){
    message("Getting cached inverse matrix")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data,...)
  x$setInverseMatrix(inverseMatrix)
  inverseMatrix
}