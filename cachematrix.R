## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix than compute it repeatedly (there are also alternatives to 
## matrix inversion that we will not discuss here).

## This function creates a special "matrix" object that can cache its inverse.
## This function is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has
## not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <-x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

