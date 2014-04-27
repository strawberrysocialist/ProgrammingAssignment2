## This script uses a function and closures to mimic an object in R 
## and enable persistence for computationally expensive operations.

## This function is a constructor that creates the object. 
## This object is implemented as a list with the list members being
## the object's assessor/setters and methods.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  #Stores whether the matrix has been changed, initialized to FALSE.
  new <- FALSE
  set <- function(y) {
    x <<- y
    i <<- NULL
    #Set the flag since the matrix has changed.
    new <<- TRUE
  }
  get <- function() x
  is.new <- function() new
  setinverse <- function(solve) {
    i <<- solve
    #Reset the flag as the inverse is known for the current matrix.
    new <<- FALSE
  }
  getinverse <- function() i
  list(set = set, get = get, 
       is.new = is.new, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function implements cached access to the result of a
## computationally expensive operation, in this case the 
## inverse of a matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  new <- x$is.new()
  if(!is.null(i) & !new) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
