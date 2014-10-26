# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

# makeCacheMatrix: creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) z <<- inverse
  getinverse <- function() z
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
# If not, it computes the inverse, sets the value in the cache via setinverse function.

# This function assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  z <- x$getinverse()
  if(!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data)
  x$setinverse(z)
  z
}

## Sample run:
x = rbind(c(2, -5), c(-5, 2))
m = makeCacheMatrix(x)
m$get()

## No cache in the first run
cacheSolve(m)


## Retrieving from the cache in the second run
cacheSolve(m)
