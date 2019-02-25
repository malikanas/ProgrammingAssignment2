## Creates a matrix and Cache its Inverse, so that it dosen't have to compute inverse again

## This Function creates matrix and cashe its inverse once computed.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y){
    
    x <<- y
    i <<- NULL
    
  }
  
  get <- function() x
  
  setInverse <- function(inverse){
    
    i <<- inverse
    
  }
  
  getInverse <- function() i
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function computes inverse of a matrix created by the above function, but if the inverse
## has already been calculated, it gets inverse from cache rather than computing again

cacheSolve <- function(x, ...) {
  
  i <- x$getInverse()
  if (!is.null(i)){
    message("Getting cashed Data")
    return(i)
    
  }
  matrx <- x$get()
  i <- solve(matrx, ...)
  x$setInverse(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
