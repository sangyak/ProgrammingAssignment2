## makeCacheMatrix creates a special matrix
## cacheSolve calculates the inverse of the matrix
## if the inverse has been calculated already, it does not calculate it again but gets the already stored value

## creates a special matrix 
## contains functions to set matrix, get matrix
## contains functions to set value of inverse, get value of inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve #what's this
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve calculates the inverse of the matrix created in makeCacheMatrix
## it first checks to see if the value has already been calculated
## if so it gives the value already calculated

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  }
