## These functions create a special "matrix" object that caches
## the inverse of a matrix. The first time the inverse requested
## it is calculated but then stored in cache thus future requests 
## of the inverse of the same matrix are immediately delivered
## instead of recalculated.


## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been 
## calculated (and the matrix has not changed), then the 
## cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    ## Return a matrix that is the inverse of 'x'
    m 
}