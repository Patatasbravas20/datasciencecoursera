## Caching Inverse of a Matrix:

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y) {
                    x <<- y
                    inv <<- NULL
          }
          get <- function() x
          setInverse <- function(inverse) inv <<- inverse
          getInverse <- function() inv
          list(set = set, get = get,
               setInverse = setInverse,
               getInverse = getInverse)
}

## Getting cached Inverse of Matrix or calculating a new one if it doesn't yet exist

cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
          inv <- x$getInverse()
          if (!is.null(inv)) {
                    message("getting cached data")
                    return(inv)
          }
          mat <- x$get()
          inv <- solve(mat, ...)
          x$setInverse(inv)
          inv
}
