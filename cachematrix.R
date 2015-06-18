## makeCacheMatrix() and cacheSolve() ## 
## 2 functions for storage and inversion of invertible matrices. 

## makeCacheMatrix function ##
## stores a matrix (x) and caches its inverse when computed by cacheSolve(x).

makeCacheMatrix <- function(x = matrix()) {
      im <- NULL
      set <- function(y) {
            x <<- y
            im <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) im <<- solve
      getsolve <- function() im
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)      
}

## cacheSolve function ##
## Takes a list returned by makeCacheMatrix() as argument x. 
## Computes the inverse of a matrix provided by x, which is then stored
## in x for future use of cacheSolve(). 
## When reused with the same argument x, the inversed matrix is directly returned
## from the cache without compution.

cacheSolve <- function(x, ...) {
       im <- x$getsolve()
      if(!is.null(im)) {
            message("getting cached data")
            return(im)
      }
      data <- x$get()
      im <- solve(data)
      x$setsolve(im)
      im
}