## The functions in this file provide a cached matrix inverse feature.
##
## First call makeCacheMatrix() with your matrix to create a cache for
## the inverse of the matrix.  Example:
##
## > m <- matrix(c(4, 7, 3, 6), ncol = 2)
## > cm <- makeCacheMatrix(m)
##
## Call cacheSolve() whenever the inverse of the matrix is needed.  It
## will only be calculated once.  Example:
##
## > inverse <- cacheSolve(cm)
## > inverse
##           [,1]      [,2]
## [1,]  2.000000 -1.000000
## [2,] -2.333333  1.333333

## makeCacheMatrix: Create a cache for the given matrix which will be
##                  used to void repeated calcuation of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: Find the inverse of the matrix.  The inverse will
##             be solved only once.  This takes as input a cache-matrix
##             returned by makeCacheMatrix().

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
