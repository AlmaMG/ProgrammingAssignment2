## This pair of functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) Inv <<- solve
  getsolve <- function() Inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the spetial "matrix" returned by 
## the above function or retrieve the inverse from the cache if it has already
## been calculated

cacheSolve <- function(x, ...) {
  Inv <- x$getsolve()
  if(is.null(Inv)) {
    message("getting cache data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setsolve(Inv)
  Inv
        ## Return a matrix that is the inverse of 'x'
}
