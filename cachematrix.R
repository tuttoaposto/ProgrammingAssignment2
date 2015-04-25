## Put comments here that give an overall description of what your
## functions do
##This script contains two functions:
##1. makeCacheMatrix(): Store input matrix and its inverse
##2. cacheSolve(): Output inverse of matrix. If inverse is not available,
##                 it calculates it.


## Write a short comment describing this function
## This function returns a list of 4 functions for the next function to work:
## set(y): Update input matrix and initialize its inverse
## get():  Store input matrix
## setinvM(inverse): Called by cacheSolve() to update inverse of matrix
## getinvM(): Store updated inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  #Initialize inverse of input matrix
  inv <- NULL
  
  
  set <- function(y) {
          x <<- y       #Change input matrix
          inv <<- NULL  #Initialize inverse of new matrix
  }
  get <- function() x #Copy final matrix
  setinvM <- function(inverse) inv <<- inverse #Update with calculated inverse, called by cacheSolve()
  getinvM <- function() inv #Copy inverse of matrix. Not updated until setinvM() is updatded
  list(set = set, get = get,
       setinvM = setinvM, 
       getinvM = getinvM)
}


## Write a short comment describing this function
## This function takes a list of 4 functions passed from makeCacheMatrix() as the arguement
## Print stored inverse if calculated
## Otherwise, solve the input matrix, output solved matrix to console, and update inverse
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinvM()  #Pass getinvM() from the list returned by makeCacheMatrix()
  if (!is.null(inv)) {
      message('get cached inverse of matrix')
      return(inv)     #If inv is not updated, just print it out
  }
  input <- x$get() #If inv is NULL, input matrix to calculate inverse
  inv <- solve(input)
  #Actions:
  x$setinvM(inv) #1. Now update setinvM() with new 'inverse'
  return (inv)   #2. Print solved matrix to console
}
