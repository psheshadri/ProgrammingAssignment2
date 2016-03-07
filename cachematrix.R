## This is a code to cache the inverse of a matrix
## MakeCacheMatrix uses a function to store a matrix, compute the inverse and store it

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Check inverse of the given matrix,
## If the inverse has been calculated (and the matrix is the same), 
## then the cachesolve should retrieve the inverse from the cache. 
## If the inverse has not been calculated, data gets the matrix stored with makeCacheMatrix 
## m calculates the inverse, and x$setmean(m) stores it in the object m in makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
