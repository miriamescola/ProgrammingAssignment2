## These functions cache the inverse of a matrix
## We consider that the matrix is always invertible

## This function creates a special "matrix" object that can cache its inverse
## The object returned is a list containing functions to get and set 
## the matrix and to get and set the inverse of the original matrix

makeCacheMatrix <- function(x = matrix()) {
  ##When the object is created, the inverse matrix haven't been calculated yet
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(a = numeric()) inv <<- a
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}