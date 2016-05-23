## Computing of inverse of the matrix and its cached value

## Function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) { ## set the matrix
x <<- y
inv <<- NULL
}
get <- function() x  ## get the matrix
setinv <- function(inver) inv <<- inver
getinv <- function() inv
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
}


## Returning of the matrix that is inverse of 'x'
## Checking if already calculated then fetching cached value
cacheSolve <- function(x, ...) {
        ## taking as input output of makeCacheMatrix
  inv <- x$getinv() 
  if(!is.null(inv)) { 
    ## getting cached value of inverse matrix and skipping the computation
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...) ## otherwise calculating inverse matrix
  x$setinv(inv) 
  inv
}

