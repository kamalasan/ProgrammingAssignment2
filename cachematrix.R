## The function calls for creating a special matrix that can hold inverse cache

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  set <- function(y) {
    ## check if the same matrix not being recalled. If not the same matrix, assign 'x"  with new matrix value
    
    if(!identical(x,y)){
      x <<- y
      
      ## then clear the "inv" variable 
      inv <<- NULL     }
   
  }
  
  get <- function() {
    x
  }
  
  ## store the inversed matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## retrieve the inversed matrix
  getInverse <- function() {
    inv
  }
  
  ## Register public methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## compute the inverse of the special "matrix" returned by makeCacheMatrix if the cache is null

cacheSolve <- function(x, ...) {

  
  inv <- x$getInverse()
  
  ## if cached is not null, then just return the cached value
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## compute matrix inversing
  mat <- x$get()
  inv <- solve(mat, ...)
  
  ## then store the value in cache
  x$setInverse(inv)
  
  
  return(inv)

}
