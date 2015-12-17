## Caching of inverse matrices to avoid long computation times  


## makeCacheMatrix creates a "cached matrix" object which
## stores the values of the underlying matrix (x) and 
## its inverse matrix (inv) in a cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() { x }
  
  setinv <- function(inv_matrix) { inv <<- inv_matrix }
  
  getinv <- function() { inv }
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve tries to fetch the inverse matrix from the cache 
## (by calling the getinv() fucntion of the "cached matrix" (x)). 
## If the inverse matrix already exists in the cache, 
## then the function returns it, otherwise, it calculates it
## and stores it in cache (by calling setinv() function 
## of the "cached matrix" object (x)).

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  
  data <- x$get()
  
  inv <- solve(data, ...)
  
  x$setinv(inv)
  
  inv
}
