## Put comments here that give an overall description of what your
## functions do

## Take a matrix and create a list of functions to support
## caching its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## Set inverse to null to show it's not been calculated
    i <- NULL
    
    # Allow for setting x to be a new matrix
    set <- function(y) {
        x <<- y
        # Inverse not yet calculated
        i <<- NULL
    }
    
    # Use to retrieve the matrix
    get <- function() x
    
    # Cache the inverse of the matrix
    setinverse <- function(inverse) i <<- inverse
    
    # Retrieve the inverse of the matrix (or NULL if uncalculated)
    getinverse <- function() i
    
    ##Return a list of functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}




## Retrieve the inverse of a matrix from the cache, or if not yet
## cached calculate and cache the inverse
cacheSolve <- function(x) {
    # Check to see if inverse has been cached (i is not NULL)
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        # Use the cached inverse
        return(i)
    }
    
    # Cached inverse not available: claculate it
    data <- x$get()
    m <- solve(data)
    # Cache the result
    x$setinverse(m)
    # Return the inverse
    m
}

