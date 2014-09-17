## these functions control the return of the inverse of a matrix.
## if a matrix inverse has already been computed and cached, these
## functions can skip further computation and return the cached inverse

## creates a list of 4 functions that set the matrix, get the matrix, set
## the matrix inverse, and get the matrix inverse to and from the cache

makeCacheMatrix <- function(x = matrix) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse <<- inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## checks to see if the inverse is already in the cache, if it is returns the
## matrix inverse, if not, calculates the inverse, writes it to the cache, 
## and returns it

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  dat <- x$get()
  inverse <- solve(dat, ...)
  x$setinverse(inverse)
  inverse
}
