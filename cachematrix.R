## These functions work together to cache the inverse of a matrix.
## If the matrix has not changes the cache will be returned and
## the high cost of calculating the inverse avoided.

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      m_inv <- NULL
      set <- function(y) {
            x <<- y
            m_inv <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m_inv <<- solve 
      getinverse <- function() m_inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function computes the inverse of the matrix created
## and if it has not changed returns the cache version

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m_inv <- x$getinverse()
      if(!is.null(m_inv)) {
            message("getting cached data")
            return(m_inv)
      }
      data <- x$get()
      m_inv <- solve(data, ...)
      x$setinverse(m_inv)
      m_inv
}
