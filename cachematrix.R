## A pair of functions that cache the inverse of a matrix
## Usage:
## To create a "Matrix" : yl <- makeCacheMatrix(x) ,
##              where x is a matrix,assume that x is always invertible
## To newly cache/retrieve the inverse of the "matrix" : cacheSolve(yl,...), 
##              where  the '...' is just for function solve()
## To make another cached  matrix :
##   yl$set(z), where z is a predefined invertible matrix 
##   cachesolve(yl)
## R version 3.5.0

## Creates a special "matrix", which is really a list containing a function to 
##   set the value of the matrix
##   get the value of the matrix
##   set the inverse of the matrix
##   get the inverse of the matrix
## Note : make sure that x is always invertible

makeCacheMatrix <- function(x = matrix()) {
  inverse_x <- NULL
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse_x <<- inv
  getinverse <- function() inverse_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
##    then cacheSolve should retrieve the inverse from the cache.
## Use the solve(X) fuction to calculate the inverse ,and the "..." is for solve()
## Note : make sure that matrix involved is always invertible


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
