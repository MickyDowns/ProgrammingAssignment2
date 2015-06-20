## Two functions (makeChacheMatrix and cacheSolve) receive a matrix, store it 
## in memory, calculate its inverse and store its inverse.

## First function (makeCacheMatrix) receives a matrix, stores it in memory, and 
## returns a list of functions that can be performed relative to that matrix.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(solve) inv <<- solve
     getinv <- function() inv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## Second function (cacheSolve) receives a matrix memory location and list of
## functions that can be performed on the matrix. If it finds that the inverse
## of the matrix has already been calculated, it returns that inverse matrix.
## Otherwise it calculates and returns the inverse of the matrix. 

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

