## As requested, this program creates a matrix object that can be cached. This matrix is
## assumed to be invertible, so no checks are made to verify this. This matrix is fed into
## makeCacheMatrix, whereas cacheSolve is used to solve for the inverse of the matrix in
## makeCacheMatrix.

## This function allows the matrix to be stored, as well as changed, if necessary.
## By storing the value of the matrix's inverse with this function, time can be saved
## if the matrix isn't changed.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) m <<- inverse
    
    getInverse <- function() m
    
    list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This is the function that actually solves for the matrix's inverse. First, it calls
## makeCacheMatrix to see if the inverse has already been solved. If this value doesn't
## exist, it solves for the matrix's inverse, and then stores it in the object created
## by makeCacheMatrix (as the matrix's inverse).

cacheSolve <- function(x, ...) {
    
    m <- x$getInverse()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    
    m <- solve(data, ...)
    
    x$setInverse(m)
    
    m
    
}
