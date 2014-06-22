## Purpose: Cache the value of the inverse of a matrix so that when we need it again, it can be looked up in the cache rather than recomputed.

## Description of this function: Creates a special "vector", which is really a list containing a function to
##      set the value of the vector
##      get the value of the vector
##      set the value of the inverse of a matrix
##      get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(mean) m <<- mean
        getSolve <- function() m
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## Description of this function:Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- Solve(data, ...)
        x$setSolve(m)
        m}
