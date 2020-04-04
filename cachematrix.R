## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Below we are creating two functions which are,
## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix.

## makeCacheMatrix is a function which creates a special matrix object 

makeCacheMatrix <- function(x = matrix()) {
  
  ab <- NULL
  set <- function(y) {
    x <<- y
    ab <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) ab <<- inverse
  get_inverse <- function() ab
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function

## cacheSolve is a function which calculates the inverse of a special matrix 
## returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ab <- x$get_inverse()
  if(!is.null(ab)) {
    message("getting cached result")
    return(ab)
  }
  data <- x$get()
  ab <- solve(data, ...)
  x$set_inverse(ab)
  ab
}


