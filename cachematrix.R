## This package provides a encapsulation of a matrix, automatically caching the
## result of inverse operation.

## Creates a "matrix" object with inverse cache enable. The following functions
## should be used on the resulting object:
## o$get - retreives the actual matrix object
## o$set - modifies the contained matrix object
## inverse(o) operation should be done through cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(inverse) inv <<- inverse
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Returns the inverse of the matrix with inverse cache. X should have been
## created with the function makeCacheMatrix. If the matrix has already been
## solved, then the cache value will be returned, otherwise it will be
## computed, cached and returned.

cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if(!is.null(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}
