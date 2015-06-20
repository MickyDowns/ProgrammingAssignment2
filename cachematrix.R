## Two functions:

## 1. makeCacheMatrix receives a matrix, stores it in memory, and returns
## a list of functions that can be performed relative to that matrix.

makeCacheMatrix = function(x = matrix()) {
     
     inv = NULL
     set = function(y) {
          x <<- y
          inv <<- NULL
     }
     
     get = function() x
     
     setinv = function(solve) inv <<- solve
     
     getinv = function() inv
     
     list(set = set, 
          get = get,
          setinv = setinv,
          getinv = getinv)
}

## 2. cacheSolve receives a matrix memory location and list of functions. If it 
## finds that the inverse has already been calculated, it returns that inverse.
## Otherwise it calculates and returns the inverse. 

cacheSolve <- function(x, ...) {
     
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

