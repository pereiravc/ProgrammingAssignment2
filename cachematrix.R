## This file contains two functions: makeCacheMatrix and cacheSolve.
## These functions are used to generate, store, and retrieve the inverse
## of a non-singular matrix to/from the memory, thus saving computation time.

## This function generates a list of 4 functions that either sets/retrieves
## a matrix or sets/retrieves its inverse to/from the memory.
makeCacheMatrix <- function(x = matrix()) {

  # Initialize the matrix inverse as a NULL object
  m <- NULL
  
  # set function: saves the input matrix to the cache, and erases
  # the matrix inverse from the cache, if any was previously saved.
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  
  # get function: returns the cached matrix to the calling code, if any.
  get <- function() x
  
  # setinv function: saves the input matrix inverse to the cache.
  setinv <- function(inv) m <<- inv
  
  # getinv function: returns the cached inverse matrix to the calling code, if any.
  getinv <- function() m
  
  # output a list of available functions.
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## This function returns the inverse of a matrix to the calling code.
## If its inverse has already been computed, the cached matrix is returned;
## if not, then the matrix is inverted and saved to memory for future reference.
cacheSolve <- function(x, ...) {
  
  # Try to get the inverse of the matrix
  m <- x$getinv()
  
  # If the inverse is cached, return it to the calling code.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # If the inverse is not cached, generate it and save to the cache.
  # Return the new matrix inverse to the calling code.
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
