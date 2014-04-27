############################
## cachematrix.R
## Date: 24/04/2014
############################

## Function: makeCacheMatrix
## Creates a "matrix" object and provides the ability to get and set the matrix as well as the associated inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## Function: cacheSolve
## Checks to see if the matrix is stored in the cache and retrieves the inverse value. Otherwise it calculates the inverse of x.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  
  # Checks the cache for the matrix and returns it if it exists.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # If it does not exist, the inverse of x is calculated and returned.
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}