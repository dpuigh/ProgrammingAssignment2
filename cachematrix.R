## Functions calculate and cache inverse of matrix
## Matrix is assumed to always be invertible

## Creates special matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns inverse of the special matrix from makeCacheMatrix
## Inverse calculated if not already cached

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("Getting cached inverse of matrix")
        return(inverse)
    }

    data <- x$get()
    message("Calcultating inverse of matrix")
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
