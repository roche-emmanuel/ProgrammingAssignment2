## The functions below will be used the cache the computation of the
## inverse of a given matrix.
## Note that we assume here that any input matrix will be invertible
## results are undefined otherwise.


## Helper function used to create an encapsulation object for our
## matrix inverse cache.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(imat) inv <<- imat
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## Function used to retrive the currently cached matrix inverse
## from a cacheMatrix object, or to compute it if not computed yet.
cacheSolve <- function(x, ...) {
  # Check if we already have a cached matrix inverse:
  inv <- x$getinverse()
  if(!is.null(inv)) {
    #message("Getting cached matrix inverse")
    return (inv)
  }
  
  # No cached matrix inverse, so we should compute it:
  inv <- solve(x$get())
  x$setinverse(inv)
  inv
}
