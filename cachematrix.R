## The functions in this file are intended to expedite matrix processing.
## They speed up calculation of the inverse of a matrix by
## keeping (caching) results and reusing them if the same matrix
## inverse is required again.
## "For this assignment, assume that the matrix supplied is always invertible."
## So there is no error checking, even though that's a terrible idea.

## This function creates a list of functions to
## get and set a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of a matrix
## and returns a saved ('cached') value, if available

cacheSolve <- function(x) {
  inv <- x$getinverse() # inv is the inverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matr <- x$get()
  inv <- solve(matr) # use solve to get inverse
  x$setinverse(inv)
  inv
}
