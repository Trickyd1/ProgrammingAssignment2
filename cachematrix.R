## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve : This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##              If the inverse has already been calculated, and is available from the cache, it retrieves it from there

## Create a cache to store the inverse of a matrix

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


## Return a matrix that is the inverse of 'x', checking first if the inverse is available from the cache

cacheSolve <- function(x, ...) {
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
