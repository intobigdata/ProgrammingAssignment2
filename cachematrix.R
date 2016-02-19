## Data Science - Programming in R ##
## Assingment 2 ##

## makeCacheMatrix() 
## creates a special "matrix" object that can cache its inverse.
## This object is a list of functions to set/get the matrix.

## cacheSolve()
## computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y     # sets x outside scope
            inverse <- NULL
      }
      get <- function() x
      setinverse <- function(inv) inverse <<- inv
      getinverse <- function() inverse
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
    
cacheSolve <- function(x, ...) {
      inverse <- x$getinverse()
      if(!is.null(inverse)) {
            message("getting cached inverse")
            return(inverse)
      }
      ## inverse not cached
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      inverse
      ## Return a matrix that is the inverse of 'x'
}
