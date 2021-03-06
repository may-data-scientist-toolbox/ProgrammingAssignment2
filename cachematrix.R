##
## repo: may-data-scientist-toolbox/ProgrammingAssignment2
## I have two functions, makeCacheMatrix and cacheSolve, to get an
## invertible matrix, calculate its inverse and cache the inverse
## calculation.
## 
## 
## makeCacheMatrix --- This function sets and gets the matrix,
##                    and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## 
##
## cacheSolve --- This function gets the matrix from above and 
## gets the cached (already calculated) inverse of this matrix, 
## skipping the calculation of inverse again.
## 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
