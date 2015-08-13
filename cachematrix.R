## These functions provide an implementation of a matrix that caches the value of its inverse

## Creates the special cache matrix. Returns a list containing functions for getting and setting the matrix, 
## and functions for getting and setting the inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Returns the inverse of the provided cache matrix and caches it. Will return the cached value on all subsequent calls
## for the same cache matrix. If the matrix has no inverse, the value NA is cached.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  inverse = tryCatch({
    solve(data, ...)
  }, error = function(e) {
    warning("Matrix has no inverse!")
    return(NA)
  })
  
  x$setInverse(inverse)
  inverse
}
