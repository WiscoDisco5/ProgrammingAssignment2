## Function for caching inverse of matrix
## Running cacheSolve on a makeCacheMatrix will search for previously calculated
## inverse and return that value rather than calculating the inverse again.

## List of functions that effectively allow for storing and retrieving matrix 
## and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function() {inv}
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Inverts matrix by either solving it directly or finding previously calculated
## inverse.

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