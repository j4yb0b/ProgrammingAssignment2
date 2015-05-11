## The following two functions are designed to cache time consuming calculation
## of the inverse of quadratic matrices. To this end, a special object is 
## created, that is capable of caching the inverse.

## Given a matrix, function `makeCacheMatrix` sets up a special matrix object
## that is basically a list capable of caching the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  set_inv <- function(inverse) inv <<- inverse
  get_inv <- function() inv
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}

## function `cacheSolve` checks whether the inverse was already calculated 
## before and if the inverse is cached. Therefore, the functions avoids 
## multiple calculation of the same inverse.

cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inv(inv)
  
  inv
}
