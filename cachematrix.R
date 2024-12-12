## The following functions create an object that store a matrix and cache 
## its inverse

## This function creates a list containing functions to set the value of the matrix,
## get the value of the matrix, set the value of the inverse matrix, get the value of the inverse
## matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## the following function computes the inverse of the matrix, but first check 
## if it had been already computed; if so it gets the inverse from the cache. Otherwise
## it computes the inverse and set the value in the cache through the setinverse() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}



