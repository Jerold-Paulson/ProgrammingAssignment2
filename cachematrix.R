## Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit 
##  to caching the inverse of a matrix rather than compute it repeatedly 
##  (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

## Write the following functions:
  
##  1. makeCacheMatrix: This function creates a special "matrix" object 
##    that can cache its inverse.

## Put comments here that give an overall description of what your
## functions do

## There are two functions in this file.  
## makeCacheMatrix creates
##   a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
##

## Write a short comment describing this function

## makeCacheMatrix is modeled on makeVector as shown in the project assignment.
## It has get,set, getInverse and setInverse methods.

makeCacheMatrix <- function(x = matrix()) {
## here is the code for the makeCacheMatrix function which will create a special 
##   'matrix' object that can cache its inverse
  inverse <- NULL ## inverse is initialized to NULL
  set <- function(y) { 
## value of special matrix is set to whatever is provided as input to the set call    
    x <<- y 
## whenever set is called, inverse is initialized to NULL    
    inverse <<- NULL
  }
## get function returns the value of the matrix, x
  get <- function() x
## setinverse function calls the solve routine to obtain the inverse of the matrix
  setInverse <- function(solve) inverse <<- solve
## getinverse function returns the value of inverse
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}
## Write a short comment describing this function
## the cacheSolve function checks the special matrix object to see if the inverse
## has already been calculated.  If it has, (i.e. non-null),
## it's retrieved from cache.
## if it hasn't, it's calculated and then stored in the cache.
cacheSolve <- function(x, ...) {
## the inverse is obtained by making a call to the getinverse method of the x object
  inverse <- x$getinverse()
## first check to see if the inverse has been already calculated
  if(!is.null(inverse)) {
## if it has, make a note of that fact by printing out a message,    
    message("getting cached data")
## ... and then return the previously cached value    
    return(inverse)
  }
## we get here if the inverse hasn't already been cached. So,
## first we gather the data to calculate the inverse - call the get method
  data <- x$get()
## now we obtain the inverse of the matrix by calling the solve function
  inverse <- solve(data, ...)
## lastly we set the cache - invoke the setinverse method of the x object
  x$setInverse(inverse)
## display the inverse that we calculated.
  inverse
}