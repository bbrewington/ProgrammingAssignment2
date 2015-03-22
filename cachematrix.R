## Coursera R Programming, Course ID rprog-012, Programming Assignment 2
## Author: Brent Brewington, https://github.com/bbrewington

## Cache the inverse of a matrix, using these 2 functions: makeCacheMatrix & cacheSolve

## makeCacheMatrix is a function that accepts a matrix input, and outputs a list containing 
## 4 different objects representing function calls.  This list object is used in another
## function "cacheSolve"

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  inv_orig <- NULL
  if (!is.matrix(x)) {
    print("Input must be a matrix")
    return(NULL)
  }
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve accepts a list object created using the function "makeCacheMatrix", and
## calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}