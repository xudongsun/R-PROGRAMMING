## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() 
    x
  setInverse <- function(Inverse) 
    Inv <<- Inverse
  getInverse <- function()
    Inv
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInverse()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  mat <- x$get()
  Inv <- solve(mat, ...)
  x$setInverse(Inv)
  Inv
}
