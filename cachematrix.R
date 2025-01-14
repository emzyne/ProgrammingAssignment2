## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(theinv) inv <<- theinv
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
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

theMatrix <- matrix(c(8,5,6,7), nrow = 2, ncol = 2)
myMatrix <- makeCacheMatrix(theMatrix)
cacheSolve(myMatrix)
cacheSolve(myMatrix)