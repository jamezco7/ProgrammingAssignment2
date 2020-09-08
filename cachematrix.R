## Week 3
## Programming Assignment 2 lexical scoping
################################################################################

## This function creates a special "matrix" object that can cache its inverse
## For this assignment, assume that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL      
    set <- function(y) {# Closures (functions written by functions) get their 
                        # name because they enclose the environment of the parent
                        # function and can access all its variables.
        x <<- y         
        inv <<- NULL
    # Unlike the usual single arrow assignment (<-) that always works on the
    # current level, the double arrow operator can modify variables in parent levels.
    }
    get <- function() {x}
    setInverse <- function(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

################################################################################
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
