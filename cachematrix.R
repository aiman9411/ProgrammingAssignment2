## The functions are used to create an object in order to store a 
## matrix and caches its inverses. The first function which is
## makeCatcheMatrix creates a special matrix.


## The second function which is cacheSolve computes the inverse of 
## the special matrix returned by the first function. If the inverse
## has been calculated, supposedly the second function retrieve the
## inverse from the cache. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
