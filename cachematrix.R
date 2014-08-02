## makeCacheMatrix(x) returns a list of functions which set
## and retrieve a matrix or a cached value, for a matrix argument
## x.
## cacheSolve(x, ...) returns a cached value from x if it exists,
## otherwise computes the inverse matrix of the set matrix, sets
## the cache value and returns the inverse.

## A list of options for an internal matrix, to
## set or retrieve the internal matrix and a cached value.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  setinv <- function (inverse) inv <<- inverse
  getinv <- function () inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Retrieve or compute a cached value of x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
