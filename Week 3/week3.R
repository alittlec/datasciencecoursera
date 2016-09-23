# Matrix inversion is usually a costly computation 
# and there may be some benefit to caching the inverse 
# of a matrix rather than compute it repeatedly

# This function creates a special "matrix" object that 
# can cache its inverse and returns a list containing a function
# to:

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  
  
  inv <- NULL #inv is the cached inverse matrix
  set <- function(y) { #set matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x  #return matrix
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, #returned list with named methods
       setinverse = setinverse,
       getinverse = getinverse)
}


# Computes the inverse of the special "matrix" 
# returned by makeCacheMatrix. If the inverse has already 
# been calculated (and the matrix has not changed), then the 
# cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}