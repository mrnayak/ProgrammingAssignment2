## makeCacheMatrix function creates a special matrix that can cache its inverse
##
## cacheSolve: This function computes the inverse of the special "matrix" 
##             returned by makeCacheMatrix above. If the inverse has already
##             been calculated (and the matrix has not changed), then the
##             cachesolve should retrieve the inverse from the cache.

## makeCacheMatix creates a special matrix from input matrix, 
##                This function helps in maintaining cache variable.
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
  
  #Set inverse
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve function calculates inverse of special matrix, 
##            If inverse is not cached then new inverse is calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  
  if(!is.null(inv))
  {
    message("getting cached data of inverse")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}