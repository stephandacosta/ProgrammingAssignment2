## Enable caching of matrix inverse operation

## function to create special object with getters and setters for given matrix and its inverse stored in closure
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
      x <<- y
      i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) {
      i <<- inverse
  }
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## function to return inverse of special object's matrix
## returns from either cache if available or calculates it and stores if not available

cacheSolve <- function(x) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}

