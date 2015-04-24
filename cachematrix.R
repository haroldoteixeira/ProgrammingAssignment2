## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function caches a matrix and keeps it´s inverse
## matrix pre-calculated to gain performance

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function (y) {
        x <<- y
        i <- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list (set=set, get=get, 
          setinverse=setinverse,
          getinverse=getinverse)
}


## Write a short comment describing this function
## Verifies is the computation of inverse matrix is 
## already done, if not, evaluates the value using the
## solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
            message("Getting cached data");
            return(i);
        } else {
            data <- x$get()
            i <- solve(data, ...)
            x$setinverse(i)
        }
        i
}
