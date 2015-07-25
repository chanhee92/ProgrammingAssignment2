## This pair of functions calculate, cache and retrieve the inverse
## of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y = matrix()) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    set.Solved <- function(inv_matrix) inv <<- inv_matrix
    
    get.Solved <- function() inv
    
    list(set = set, get = get,
        set.Solved = set.Solved,
        get.Solved = get.Solved)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been
## calculated (and the matrix has not changed), then cacheSolve
## retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv <- x$get.Solved()
    if (!is.null(inv)) {
        message("getting cached data...")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$set.Solved(inv)
    return(inv)
}
