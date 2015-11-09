# This file contains tests for the cachematrix implemenetations

# Helper function used to create an upper triangular matrix 
# Should always be invertible (if the diagonal coeffs are not zeros)
# see for instance: https://www.ashleymills.com/node/319
# This function takes the desired number of rows (== num of columns)
# => Actually this dosen't work very well and we get singularities...
makeUTMat <- function(nrows)
{
  # ensure we have enough rows:
  if(nrows < 1) {
    stop("Invalid number of rows")
  }
  
  # Create an empty vertor of data:
  data <- numeric(0)
  
  # iterate on each column:
  for(i in 1:nrows) {
    col <- c(rnorm(i-1),1,rep(0,nrows-i))
    data <- c(data,col)
  }
  
  # create a matrix from the vector data:
  matrix(data,nrows,nrows)
}

# Helper function used to create a covariance matrix
# => should always be invertible ?
# Same limitation as before: system computationally singular!
makeCovMat <- function(nrows)
{
  # ensure we have enough rows:
  if(nrows < 1) {
    stop("Invalid number of rows")
  }
  
  
  mat <- matrix(0.0,nrows,nrows)
  
  for(i in 1:10) {
    vec <- rnorm(nrows)
    mat = mat + (vec %*% t(vec))*100.0
  }
  
  # Return the computed matrix:
  mat 
}

# Method to generate arandom square matrix,
# Should be invertible according to:
# http://stackoverflow.com/questions/19106015/r-how-to-generate-random-yet-easily-invertible-matrices
makeRandom <- function(nrows) 
{
  matrix(rnorm(nrows*nrows),nrows,nrows)
}


# Helper method used to compute the inverse of a random
# upper triangular matrix of a given size, for a  
# given number of times.
# may also used the cacheMatrix implementation:
# will also return the computed inverse.
computeInverse <- function(nrows,num, useCache=FALSE)
{
  #mat <- makeUTMat(nrows)
  #mat <- makeCovMat(nrows)
  mat <- makeRandom(nrows)
  
  inv <- NULL
  func1 <- function() {
    cmat <- makeCacheMatrix(mat)
    for(i in 1:num) {
      inv <<- cacheSolve(cmat)
    }
  }
  
  func2 <- function() {
    for(i in 1:num) {
      inv <<- solve(mat)
    }
  }
  
  t1 <- system.time(func1())
  
  t2 <- system.time(func2())
  
  
  message(sprintf("Acceletation factor: %f",if(t1[3] > 0.0)  t2[3]/t1[3] else NaN))  

  invisible(inv)
}

