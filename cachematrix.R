## Put comments here that give an overall description of what your
## functions do

## Creates a matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinversed <- function(inversed) i <<- inversed
  getinversed <- function() i
  list(set = set, get = get,
       setinversed = setinversed,
       getinversed = getinversed)

}


## Returns the inverse matrix of x from cache if exists in the cache, otherwise creates
## the inverse matrix, save it to the cache, then return it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinversed()
  if(!is.null(i)) {
    message("getting cached inversed matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinversed(i)
  i
}
