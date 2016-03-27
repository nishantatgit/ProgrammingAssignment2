# makeCahcheMatrix function initializes a matrix and returns a list of functions to operate 
# on it like setting its values, and computing inverse. cacheSolve function returns the inverse 
# of the matrix created by makeCahcheMatrix function. If inverse does not exist then
# it first calculates and caches it. 



#makeCahcheMatrix initializes a matrix which can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  
  inv <- NULL
  
  set <- function(y)
  {
    if(ncol(y) != nrow(y))
    {
      message("Error : y is not an square matrix")
      return
    }
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x 
  
  setInverse <- function(i) inv <<- i
  
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## This fuinction takes a matrix as an argument and returns the cached inverse 
## if that exits, otherwise calculates and returns the inverse of matrix after 
## caching it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  invrs <- x$getInverse()
  
  if(!is.null(invrs))
  {
    message("getting cached matrix")
    return(invrs)
  }
  
  data <- x$get()
  
  invrs <- solve(data, ...)
  
  x$setInverse(invrs)
  
  invrs
}

