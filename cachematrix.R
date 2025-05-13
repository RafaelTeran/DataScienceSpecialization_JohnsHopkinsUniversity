## These functions work together to calculate the inverse of a matrix
## and cache (store) the result to avoid redundant computations.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # will store the cached inverse
  set <- function(y) {
    x <<- y       # assign new matrix
    inv <<- NULL  # reset cached inverse
  }
  get <- function() x                    # return the matrix
  setinverse <- function(inverse) inv <<- inverse  # store the inverse
  getinverse <- function() inv           # return the cached inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed,
## it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  # check if inverse is already cached
  if(!is.null(inv)) {
    message("getting cached data")  # inform that cached value is used
    return(inv)
  }
  mat <- x$get()         # get the matrix
  inv <- solve(mat, ...) # compute the inverse
  x$setinverse(inv)      # cache the inverse
  inv
}
