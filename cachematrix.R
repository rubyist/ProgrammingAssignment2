## These functions provide a method of caching matrix inversions

## makeCacheMatrix creates an object wrapping a matrix that allows the calculated
## inverse to be cached.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setsolve <- function(inv) i <<- inv
  getsolve <- function() i
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve will invert a matrix created by makeCacheMatrix and cache the results,
## returning a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {
  i <- x$getsolve()
  if (!is.null(i)) {
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setsolve(i)
  i
}
