## Returns a 'special" matrix object that caches its own inverse
## the first time it  gets calculated.

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) invx <<- solve
  getsolve <- function() invx
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Checks whether the inverse matrix of x has already been calculated
## and returns the cached copy in that case.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invx <- x$getsolve()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setsolve(invx)
  invx
}
