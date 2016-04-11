# Functions to create and manipulate a matrix object (list) that
# has its inverse matrix stored as a property

# makeCacheMatrix(<args>)
# Creates a list that emulates a matrix object that has its inverse
# matrix stored as a property
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() {
        x
    }
    
    setInverse <- function(inv) {
        inverse <<- inv
    }
    getInverse <- function() {
        inverse
    }
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# cacheSolve(<args>)
# Calculate the inverse of a CacheMatrix. Returns the value stored in the cache
# in case the inverse has already been calculated.

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    
    inverse <- x$getInverse()
    
    if(!is.null(inverse)) {
        message("Getting cached data...")
    }
    else {
        inverse <- solve(x$get())
        x$setInverse(inverse)
    }    

    inverse
}

