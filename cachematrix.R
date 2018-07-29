## Below function is to get the inverse of the passed matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  ##inverse of a matrix is getting calculated.
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##Below function gets the cached data

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  ##Checks if the inverse of the matrix already exists
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  ## If the inverse is not already calculated from the above function, CacheSolve calculates,
  ##the inverse using the solve function
  i <- solve(data, ...)
  x$setinverse(i)
  i
}