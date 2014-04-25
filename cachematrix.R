## Put comments here that give an overall description of what your
## functions do

## creates a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse of matrix
##get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  #inverse_x is to hold the inverse of matrix. Set the value of variable to NULL
  inverse_x <- NULL
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  get <- function() x
  ## Determine inverse of matrix. Inverse of matrix can be calculated using function : solve()
  setinverse <- function(inverse) inverse_x <<- solve
  ## Returns the inverse of matrix
  getinverse <- function() inverse_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## calculates the inverse of the special "matrix" created with the above function. However, it first checks to see 
## if inverse  has already been calculated. If so, it gets the inverse  from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the matrix in the cache via the 
## setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_x <- x$getinverse()
  ## Checks if inverse of matrix is already calculated. If inverse is already calculated, function jusr returns
  ## the value of inverse_x which holds inverse of matrix.
  if(!is.null(inverse_x)) {
    message("Getting cached matrix")
    return(inverse_x)
  }
  data <- x$get()
  inverse_x <- solve(data, ...)
  x$setinverse(inverse_x)
  inverse_x
}
