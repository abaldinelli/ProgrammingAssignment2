## This file defines a set of functions for creating and manage a special kind of 
## matrix that stores its inverse internally. There is some code for testing 
## proposals.

##
## This function creates an special type of matrix to cache internally its inverse.
## This type is composed by 4 functions:
## - set: sets a matrix to be stored;
## - get: returns the original matrix;
## - getInverse: returns the storedInverse value;
## - setInverse: sets the storedInverse value, and clean the cached data;
##
makeCacheMatrix = function(x = matrix()) {
  storedInverse <<- NULL
  list(set = function(matrixToSet) {
          x <<- matrixToSet
          storedInverse <<- NULL
        },
       get = function() x,
       setInverse = function(inverse) storedInverse <<- inverse,
       getInverse = function() storedInverse)
}

##
## This function returns the inverse of the given special matrix (created using previously defined makeCacheMatrix).
## - If the inverse has been already calculated, it is returned from cache;
## - In case cache is not available, then inverse of the given matrix is calculated and stored in the internal cache 
##   using the function setInverse.
##
cacheSolve <- function(x, ...) 
{
  cachedData = x$getInverse()
  if(!is.null(cachedData)) 
  {
    message("Reading from cache...")
  }
  else
  {
    message("Calculating...")
    cachedData = solve(x$get(),...)
    x$setInverse(cachedData)
  }
  cachedData
}

########################################## 
## Code for testing proposed functions
##########################################
message ("Testing...")
message ("Making New Matrix")
matrix <- makeCacheMatrix(matrix(1:4, 2))
cacheSolve(matrix) # must calculate
cacheSolve(matrix) # must call cached data
message ("Making New")
matrix$set(matrix(5:8, 2))
cacheSolve(matrix) # must calculate
cacheSolve(matrix) # must call cached data
message ("End Testing")