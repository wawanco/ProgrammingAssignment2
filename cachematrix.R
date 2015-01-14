## Matrix inversion is a very time consuming operation
## If we want to compute matrix inverse twice, functions below
## will allow to cache the result during the first computation
## and to retrieve the cached result for the second computation
## instead of performing twice the same computation.

## "makeCacheMatrix" creates a special matrix which is really a list of functions
makeCacheMatrix <- function(x = matrix()) {
    # cInverse stores the inverse of a the matrix.
    # cInverse is initialized to NULL
    cInverse <- NULL
    
    # "set" sets a new value to the internal matrix x.
    # As the matrix is altered by set, cInverse has to be reinitialized to NULL
    # in order to indicate that it needs to be re-computed
    set <- function(y) {
        x <<- y
        cInverse <<- NULL
    }
    
    # "get" returns the internal matrix
    get <- function() {
        x
    }
    
    # "setInverse" sets the inverse of the matrix
    # This is really the function that stores the result of 
    # the matrix inversion
    setInverse <- function(inverse) {
        cInverse <<- inverse
    }
    
    # "getInverse" returns the stored inverse (even if NULL)
    getInverse <- function() {
        cInverse
    }
    
    # Returned list of functions
    list(
        set = set, 
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## "cacheSolve" performs the matrix inversion of the special matrix created 
## with "makeCacheMatrix".
## However it firsts check if inverse has already been calculated. 
## If so, it returned the result in cache, otherwise it calculates the inverse.
cacheSolve <- function(x, ...) {
    # First, get the currently stored inverse
    inv <- x$getInverse()
    # If not NULL, returns the current value and exits.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # Compute the matrix inversion
    data <- x$get()
    inv <- solve(data, ...)
    # Store the result for later use
    x$setInverse(inv)
    # Return the newly computed value
    inv
}
