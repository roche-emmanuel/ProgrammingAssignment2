## The functions below will be used the cache the computation of the
## inverse of a given matrix.
## Note that we assume here that any input matrix will be invertible
## results are undefined otherwise.

## Implementation note: I deliberately uses 2 spaces indends in this code even
## if using more than 4 spaces seems to be advised in this course. Please do
## not consider this as a layout error since this is simply my personnal
## coding style (and I've been coding like that for years... :-) )


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
