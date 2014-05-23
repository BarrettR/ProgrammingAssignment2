## This file contains two functions that cache the inverse of a matrix
## The first function is makeCacheMatrix which create a special matrix object that can cache its inverse
## The second function is cacheSolve which computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated, then it is retrieved from the cache


## creates a matrix object. Function assumes that the matrix is invertible and will cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() s
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the inverse of a matrix. Retrieves the inverse from cache if it has already been calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
