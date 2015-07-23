## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y = matrix()) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    set.Solved <- function(inv_matrix) inv <<- inv_matrix
    
    get.Solved <- function() inv
    
    list(set = set, get = get, set.Solved = set.Solved, get.Solved = get.Solved)
}


## Write a short comment describing this function

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
