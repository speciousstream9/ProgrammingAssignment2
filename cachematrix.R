# This first module is used to define a special kind of matrix, it caches the result of the
# inverse matrix calculation.
#
# Matrix inversion is usually has a high computation and, thus, caching its
# result instead of computing it repeatedly is a good method to use


makeCacheMatrix <- function(x = matrix()) {
  # Create a matrix wrapper, that allows for caching the matrix inversion
  #
  # Arguments:
  #   x: The matrix to be wrapped.
  #
  # Result (Returns):
  #   A list holding the special matrix, containing functions to set and
  #   get the matrix and inverse matrix values.

  cached.inverse <- NULL
  # When setting new values to the matrix, the cached inverse should be reset
  set <- function(y) {
    x <<- y
    cached.inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cached.inverse <<- inverse
  getinverse <- function() cached.inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  # Calculates the inverse of the matrix created with the function makeCacheMatrix.
  #
  # It checks if we already have the needed values. If so, it gets
  # the inverse matrix from cache. Otherwise, it calculates the inverse of
  # the matrix and sets values to it.
  #
  # Arguments:
  #   x: The wrapped matrix, created with makeCacheMatrix
  #
  # Results:
  #   The inverse of the given matrix

  inverse <- x$getinverse()

  if(!is.null(inverse)) {
    message("Using cached data")
  } else {
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
  }
  inverse
}
