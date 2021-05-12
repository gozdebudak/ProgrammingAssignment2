## The 'makeCacheMatrix' function creates a special 
## "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Creating an NULL 'inverse' variable for keeping the inverse matrix
  inverse <- NULL
  ## The 'set' function is for setting the x and inverse variable
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  ## The 'get' function returns the x
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## 'cacheSolve' function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix function. If the inverse has already been 
##  calculated (and the matrix has not changed), the 'cacheSolve' retrieves 
##  the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- inv(data, ...)
  x$setInverse(inverse)
  inverse
}
