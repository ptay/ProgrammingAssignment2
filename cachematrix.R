## This code consists of two functions as follows:
## 1. makeCacheMatrix: creates a special "matrix" object that caches its inverse
## 2. cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix
## 
## History:
## 1. June 3, 2017 Initial version

## makeCacheMatrix: creates a special "matrix" object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
