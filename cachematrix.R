## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = numeric()) {
  
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

