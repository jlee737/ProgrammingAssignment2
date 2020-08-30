# Creates matrix that gets and sets values of 
# matrix and inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }

  get <- function() x 
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv, 
       getinv = getinv)
  
  }


## Checks to see if inverse matrix is in cache. If not, 
## recomputes value and adds to cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    return(i)}
  data <- x$get()
  i <- solve(x, ...)
  x$setinverse(i)
  i
  }
