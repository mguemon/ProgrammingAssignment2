## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly


## Function makeCacheMatrix
## This function creates a special "matrix" object 
## that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  Inv_x <- NULL
  set <- function(y) {
    x <<- y
    Inv_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) Inv_x <<- inverse
  getinverse <- function() Inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
  
  



## Function cacheSolve
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  Inv_x <- x$getinv()
  if(!is.null(Inv_x)) {
    message("getting cached data")
    return(Inv_x)
  }
  data <- x$getinverse()
  Inv_x <- solve(data, ...)
  x$setinverse(Inv_x)
  Inv_x
}
