## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function caches a matrix and keeps it´s inverse
## matrix pre-calculated to gain performance

makeCacheMatrix <- function(x = matrix()) {
    
    ## sets the initial state of the cache to NULL
    
    i <- NULL
    
    ## set function alter the global matrix x to newest
    ## matrix passed as argument
    
    set <- function (y) {
        x <<- y
        i <- NULL
    }
    
    ## get function return the current in memory matrix
    get <- function() x
    
    ## setinverse function sets the inverse matrix to 
    ## be cached. The <<- operator keeps the state
    setinverse <- function(inverse) i <<- inverse
    
    ## getinverse function returns the cached value
    getinverse <- function() i
    
    ## generates the special inmemory matrix
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
        ## uses the special vector getinverse function
        ## to return the current cached value
        i <- x$getinverse()
        
        if (!is.null(i)) {
            ## if cached value is not null, 
            ## returns the value of cached value,
            ## stored in variable i
            message("Getting cached data");
            return(i);
        } else {
            ## if cached value is null, 
            ## obtains matrix data, calculates the 
            ## inverse and store the result
            data <- x$get()
            i <- solve(data, ...)
            x$setinverse(i)
        }
        
}
