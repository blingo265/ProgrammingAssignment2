## These are a pair of functions that cache the inverse of a matrix.


# This function takes a matrix as an input and returns a list of functions ( a "special vector") as an output.
# The returned list of functions contains functions to set the matrix, get the matrix, set the value of matrix inverse,
# and get the value of matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) matinv <<- inv
  getinv <- function() matinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# This function accepts the list of functions (a "special vector") returned from makeCacheMatrix. It checks for cached value
# for the inverse matrix. If a cached value is available, it returns the cached value as the matrix inverse. If a cached value is 
# not available, then it calculates the matrix inverse, and returns the value.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matinv <- x$getinv()
  if(!is.null(matinv)) {
    message("getting cached data")
    return(matinv)
  }
  data <- x$get()
  matinv <- solve(data, ...)
  x$setinv(matinv)
  matinv
}
