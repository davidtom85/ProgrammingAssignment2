## Overall, these functions will calculate the inverse
## of a square matrix, first by checking the cache, if
## the inverse has already been calculate it will get it
## from there, if not, it will calculate it and store it
## in cache.

## makeCacheMatrix will store in cache all the previous calculated
## inverse matrices and will allow cacheSolve to pull these
## inverse matrices so they don't need to be calculated again.


makeCacheMatrix <- function(x = matrix())  {
  m <- NULL
  set <- function(y) { 
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve will calculate the inverse of the given
## matrix, either by pulling it from cache or by 
## calculating it with solve()

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}