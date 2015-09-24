## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  CachedMatrix <- NULL #initialize
  
  set <- function(y) {
    x <<- y
    CachedMatrix<<- NULL
  }
  get <- function() x
  setInverse <- function(solve) CachedMatrix <<- solve
  getInverse <- function() CachedMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  CachedMatrix<- x$getInverse()
  
  if(!is.null(CachedMatrix)) {
    message("getting cached data")
    return(CachedMatrix)
  }
  
  matrix <- x$get()
  CachedMatrix <- solve(matrix)
  x$setInverse(CachedMatrix)
  CachedMatrix
}
