## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## a list of functions that sets and gets a matrix and sets and gets the value
## of its inverse
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  
  ## if a new matrix is set, the inverse is set to NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  # returns the current matrix
  get <- function() x
  
  # sets the s variable on the parent scope
  setsolve <- function(solve) s <<- solve
  # returns the cached s value
  getsolve <- function() s
  
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # gets the cached value of solve(x) and checks if it is null
  # if it is not null, returns the cached value. if it is null, 
  # then it computes the value of the function, sets the cached 
  # value to be reused and then returns the now cached value
  
  s <- x$getsolve() 
  if (!is.null(s)) {
    s
  } else {
    data <- x$get()
    s <- solve(data)
    x$setsolve(s)
    s
  }
}
