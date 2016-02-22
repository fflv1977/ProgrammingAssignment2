## Two Functions to follow, (1) makeCacheMatrix and (2) cachesolve

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
## 0ne argument, it must be a matrix
## usage: makeCacheMatrix(x)
makeCacheMatrix <- function(x = matrix()) {
  
  ima <- NULL
  
  set <- function(y) {
    x <<- y
    ima <<- NULL
  }
  get <- function() x
  
  setsolve <- function(solve) ima <<- solve
  getsolve <- function() ima
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cachesolve: it calculates the inverse of the matrix returned by makeCacheMatrix().
## 0ne argument, it must be a function
## usage: cachesolve(f(x))

cachesolve <- function(x, ...) {
  ima <- x$getsolve()
  if(!is.null(ima)) {
    message("getting cached data")
    return(ima)
  }
  data <- x$get()
  ima <- solve(data, ...)
  x$setsolve(ima)
  ima
}

