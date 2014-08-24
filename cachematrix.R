## The two functions below implent a method to compute the inverse of a matrix
## with the special property that the inverse, once computed, is cached, so that
## subsequent calls return the cached inverse without recomputing.

## makeCacheMatrix is used to store a matrix and its inverse in a cache.
## makeCacheMatrix returns a list of possible methods

makeCacheMatrix <- function(x = matrix()) {
  xInverse <- NULL
  set <- function(y) {
    x <<- y
    xInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(xInvVal) xInverse <<- xInvVal
  getInverse <- function() xInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Cachesolve returns the inverse of the matrix. If the inverse has
## been computed before, returns the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xInv <- x$getInverse()
  if(!is.null(xInv)) {
    message("getting cached data")
    return(xInv)
  }
  data <- x$get()
  xInv <- solve(data, ...)
  x$setInverse(xInv)
  xInv
}
