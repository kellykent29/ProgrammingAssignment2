## Put comments here that give an overall description of what your 
## functions do 


## Create a matrix object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) { 
  
## Define the cached inverse
  invrs = NULL
  set = function(y) {
      x <<- y
      invrs <<- NULL
  }
  get = function () x ## Return the matrix x
  setinverse <- function(inverse) invrs <<- inverse ## Set the cache to the inverse of matrix x
  getinverse = function () invrs
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}  


 

## Computes the inverse of the matrix created by makeCacheMatrix. If the inverse
## has already been calculated, use the cached result.
 

cacheSolve <- function(x, ...) { 
   ## Return a matrix that is the inverse of 'x' 
  
  invrs = x$getinverse()
  
   ## If the inverse is already cached, use this and skip the calculation
  if (!is.null(invrs)) {
    message ("Using cached result")
    return(invrs)
  }
   # Else calculate inverse and return
  data <- x$get()
  invrs <- solve(data,...)
  x$setinverse(invrs) ## Set value of inverse
  return(invrs)
  
} 
