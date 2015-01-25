## makeCacheMatrix function creates a special matrix that can cache its inverse.
##
##  get()            : Get input matrix.
##  set(y)           : Set input matrix to special matrix, clear cache.
##  setinverse(inv)  : Set inverse of matrix, used by cacheSolve.
##  getinverse()     : Get inverse of matrix.
##
## Note : Input matrix supplied is always invertible.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  #Set matrix
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  
  #Get matrix
  get <- function() x
  
  #Set inverse of the matrix, to be used only by cacheSolve
  setinverse <- function(inverse) inv <<- inverse
  
  #Get matrix inverse
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve: This function computes the inverse of the special "matrix" 
##             returned by makeCacheMatrix above. If the inverse has already
##             been calculated (and the matrix has not changed), then the
##             cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  
  if(!is.null(inv))
  {
    message("getting cached data of matrix inverse")
    return(inv)
  }
  
  ##Get input matrix
  data <- x$get()
  
  ##Solve is used to compute matrix inverse
  inv <- solve(data, ...)
  
  x$setinverse(inv)
  
  ##Return inverse
  inv
}