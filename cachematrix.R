## 1. makeCacheMatrix and cacheSolve
## makeCacheMatrix is a getter and setter of the cache and the matrix itself

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      get <- function() x
      setmean <- function(mean) m <<- mean
      getmean <- function() m
      list(
        set = set, 
        get = get,
        setmean = setmean,
        getmean = getmean
        )
}

## 2. Cashsolve
## cacheSolve will check if the result is cached and return it, if condition is true. 
## If condition is not true it it will perform the solve function to inverse the matrix and put it in the cache

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getmean()
      if(!is.null(m)) {
        message("get cached data")
        return(m)
      }
      data <- x$get()
      ##m <- mean(data, ...)
      m <- solve(data, ...)
      x$setmean(m)
      m 
}
