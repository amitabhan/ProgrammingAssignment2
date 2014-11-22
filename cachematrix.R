## Put comments here that give an overall description of what your
## functions do

## This is a pair of functions that cache the inverse of a matrix.
## Assumption:  the matrix supplied is always invertible.

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  #  cachedInverse will store the inverse of the given matrix
  #  and it's reset to NULL every time makeCacheMatrix is called
  
  cachedInverse <- NULL
  
  # this function sets a new matrix and reset the cachedInverse to NULL
  
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  
  # this function returns the value of the original matrix
  
  get <- function() {x}
  
  # setter function for cachedInverse
  
  setinverse <- function(inverse) {cachedInverse <<- inverse}
  
  # returns value of cachedInverse
  
  getinverse <- function() {cachedInverse}
  
  # This is a list of the internal functions ('methods') 
  # so a calling function knows how to access those methods.  
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # the input x is an object created by makeCacheMatrix
  
  # accesses the object 'x' and gets the value of the inverse
  
  cachedInverse <- x$getinverse()
  
  # if inverse was already cached (not NULL) then
  # show message in the console and return the cached inverse value
  
  if(!is.null(cachedInverse)) {
    message("getting cached data")
    return(cachedInverse)
  }
  
  # this part is executed only when there is no cached inverse value
  
  # get the matrix
  
  data <- x$get()
  
  # get the inverse of the matrix
  
  cachedInverse <- solve(data)
  
  # store the inverse to the cachedInverse 
  
  x$setinverse(cachedInverse)
  
  # return the inverse matrix to the code that called this function  
  
  cachedInverse
}
