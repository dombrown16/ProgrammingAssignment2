## A pair of functions that cache the inverse of a Matrix


## This function creates a special "matrix" object hat can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y) {
              x <<- y
              inv <<- NULL
      }
get <- function() x
setInverse <- function(Inverse) inv <<- inverse
getInverse <- function() inv
list(set = set,
     get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}


## This function computes the the inverse of the special "matrix" returned
## by the CacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
