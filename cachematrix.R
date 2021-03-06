## makeCacheMatrix - setup functions to operate on object containing the supplied matrix x
## cacheSolve - will return the inverse of a matrix from a cacheable matrix created with makeCacheMatrix

## Create special cacheable matrix object with functions for key operations

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solved) i <<- solved
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Return the matrix inverse of x by solving or retrieving exisiting cache if previously solved

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
