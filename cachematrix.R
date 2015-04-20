## These 2 functions below are dedicated to create and use
## of cache of matrix inversion operation

## First function makeCacheMatrix creates a cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve # solve(X) returns matrix x inversion
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Second function cacheSolve uses result of above cached value.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

# test:
# x <- matrix(c(1,0,0,0,1,0,0,0,1),3)
# x <- matrix(c(2,0,0,0,2,0,0,0,2),3)
# y <- makeCacheMatrix(x)
# cacheSolve(y)
