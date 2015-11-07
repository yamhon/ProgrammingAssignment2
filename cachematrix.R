# This file defines two functions:
# - makeCacheMatrix() creates a special "matrix" object that can cache its 
#   inverse, if any.
# - cacheSolve() computes the inverse of the special "matrix" returned by 
#   makeCacheMatrix() above. If the inverse has already been calculated 
#   (and the matrix has not changed), then the cachesolve should retrieve 
#   the inverse from the cache.

# @return an object with 4 member functions
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                            # cached inverse
  list(setMatrix = function(y) { x <<- y; m <<- NULL } # set matrix, clear cache
       , getMatrix = function() { x }                  # get original matrix
       , setInverse = function(cache) { m <<- cache }  # cache inverse
       , getInverse = function() { m }                 # get cached inverse
       )
}

# @assume type(x) == CacheMatrix()
# @assume x is an inversible matrix, so no explicit error handling
# @return inverse of 'x' (cache if not set)
cacheSolve <- function(x, ...) {
  if (is.null(x$getInverse()))                         # check if any inverse cached
  {
    x$setInverse( solve(x$getMatrix(), ...) )          # calculate inverse and cache it
  }
  return (x$getInverse())                              # return inverse
}