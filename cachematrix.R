## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This functions makes a matrix that has the ability
# to cache the result of the solve function
makeCacheMatrix <- function(x = matrix()) {
 s  <- NULL
 # set: sets the matrix
 set  <- function(y) {
   # assign values in different environment with <<-
   x <<- y
   s <<- NULL
 }
 # get: returns the matrix
 get  <- function() x
 # setsolve: set the solution
 setsolve  <- function(solve) s  <<- solve
 # getsolve: gets the solution
 getsolve  <- function() s
 list(set = set, get = get,
      setsolve = setsolve,
      getsolve = getsolve)
}


## Write a short comment describing this function
# This function solves a cachable matrix
# using the cached result if available
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # see if the result has been cached
  s  <- x$getsolve()
  if(!is.null(s)) {
    # Getting chaced data
    return(s)
  }
  # otherwise, solve now and cache the result
  data  <- x$get()
  s  <- solve(data)
  x$setsolve(s)
  s
}
