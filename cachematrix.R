## Caching the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
## This function creates a matrix object that can cache its inverse
    im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) im <<- inverse
  getinverse <- function() im
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
      im <- x$getinverse()
    if (!is.null(im)) {
      message("Cached data")
      return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setinverse(im)
    im
}