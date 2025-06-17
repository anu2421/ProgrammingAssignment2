## This file defines two functions:
## - makeCacheMatrix: creates a special matrix object that can cache its inverse
## - cacheSolve: computes or retrieves the cached inverse of the matrix

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset inverse if the matrix is changed
  }
  
  get <- function() x  # Returns the original matrix
  
  setinverse <- function(inverse) inv <<- inverse  # Stores the inverse
  getinverse <- function() inv  # Retrieves the cached inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed),
## then the cached inverse is returned
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting cached inverse...")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)  # Compute the inverse
  x$setinverse(inv)  # Cache the inverse
  inv
}

