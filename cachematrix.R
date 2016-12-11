## This program allows the user to calculate the inverse of a matrix. 
## It also caches the matrix inverse. In case the Matrix Inverse is available 
## in cache it will return that value. Otherwise it will calculate the matrix 
## inverse, store it in cache and return the value to the user. 

## makeCacheMatrix defines an object that contains attributes and operations.
## makeCacheMatrix contains functions to allow the user to store and retrieve  
## the inverse of a matrix. It also allows the user to get the input (matrix) for 
## which the inverse is requested. 
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## Once an object of type makeCacheMatrix() is created, set() allows its value 
    ## to be changed without initializaing another instance of the object. 
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Get Matrix for which the inverse is requested 
    get <- function() x
 
    ## Store the inversion of the Matrix.
    setMatrixInverse <- function(m.inverse) m <<- m.inverse
    
    ## Retrieve the Matrix Inverse as it was stored. 
    getMatrixInverse <- function() m 
    
    ## Making sure the functions are available in the parent environment
    list(set = set, get = get,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse)
}

## cacheSolve calculates the inverse of a matrix. Before doing so it checks
## if the requested calue is available in cache. If so it returns the cached value.
## If not, it calculates the matrix inverse, stores it in makeCacheMatrix and returns
## matrix inverse to the user. 
cacheSolve <- function(x, ...) {
    m <- x$getMatrixInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setMatrixInverse(m)
    m
}
