## cachematrix.R

## Caching the Inverse of a Matrix
## Forked from github for Programming Assignment 2 for R Programming on Coursera

## Usage:
## mtx <- matrix(1:4, nrow=2, ncol=2)
## c_mtx <- makeCacheMatrix(mtx)
## cacheSolve(c_mtx)


## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setMtrxInv <- function(inverse) inv <<- inverse
    getMtrxInv <- function() inv
    list(set = set,
         get = get,
         setMtrxInv = setMtrxInv,
         getMtrxInv = getMtrxInv)
}


## Retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getMtrxInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setMtrxInv(inv)
    inv
}
