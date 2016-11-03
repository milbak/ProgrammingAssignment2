## The following functions create a matrix object, and calculate
## the inverse using the solve() function

## makeCacheMatrix() creates a special matrix object and returns 
## a list of functions that can cache the computed inverse

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(inv) i <<- inv
      getinv <- function() i
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## cacheSolve() returns the cached inverse of the matrix object
## or computes the inverse using solve() if the cache is empty

cacheSolve <- function(x, ...) {
      i <- x$getinv()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinv(i)
      i
}
