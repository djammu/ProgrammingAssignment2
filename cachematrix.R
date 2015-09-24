##makeCacheMatrix has four function
## set --> Set the matrix passed to the function
## get --> get the matrix which was set
## setInverse --> Cache the Matrix passed to this (using the << operator) 
## getInverse --> Get the Cached Matrix

##How to Run the functions (Example)
##source("cachematrix.R")
##a<-makeCacheMatrix()
##a$set(matrix(1:4,2,2))
##cacheSolve(a)
        


makeCacheMatrix <- function(x = matrix()) {

  CachedMatrix <- NULL #initialize
  
  set <- function(y) {
    x <<- y
    CachedMatrix<<- NULL
  }
  get <- function() x
  setInverse <- function(solve) CachedMatrix <<- solve ##Cache the matrix 
  getInverse <- function() CachedMatrix ##get the cached Matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The function will retrun the Inverse of the matrix passed to this
## If the Inversed matrix is available in the Cache it will return from the cache
## If the Inversed matrid is not available in the Cache it will find the Inverse 
## of the matrix(using the solve function) and set it to the Cache.

cacheSolve <- function(x, ...) {
        
  CachedMatrix<- x$getInverse() ##Check if the matrix is available in the Cache.
  
  if(!is.null(CachedMatrix)) {
    message("getting cached data")
    return(CachedMatrix) ##If available get the cached matrix
  }
  
  matrix <- x$get()
  CachedMatrix <- solve(matrix) ## find the Inverse of the matrix
  x$setInverse(CachedMatrix) ## Call the Set function to Cache the Inverse matrix
  CachedMatrix ## display the inverse matrix
}
