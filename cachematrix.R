## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Returns a list with 4 functions to enable caching
# of computed values on the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# Returns inverse of matrix, either by solve() or pulling
# previously computed inversions from cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  retval <- x$getinverse()
  if(!is.null(retval)) {
    #message("CACHE HIT!")
    return(retval)
  }
  #message("CACHE MISS!")
  # Get the uncached value
  data <- x$get()
  # Solve the inverse
  retval <- solve(data, ...)
  # Save the inverse back to the cache, then return inverse
  x$setinverse(retval)
  retval
}



# test it out
#test1 <- matrix(rnorm(100), 10, 10)
#a1 <- makeCacheMatrix(test1)
#cacheSolve(a1)
#cacheSolve(a1)
#cacheSolve(a1)

# try another
#test2 <- matrix(rnorm(10000), 100, 100)
#b1 <- makeCacheMatrix(test2)
#cacheSolve(b1)
#cacheSolve(b1)

#cacheSolve(a1)
#cacheSolve(b1)
