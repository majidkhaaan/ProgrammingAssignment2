## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## initially set the cached inverse to NULL since it is not yet calculated
        xInv <- NULL
        ## the following function sets the data into the special "matrix"
        set <- function(y) {
                x <<- y
                xInv <<- NULL
        }
        ## the following function gets the data from the special "matrix"
        get <- function() x
        ## the following function sets the inverse into the cach
        setInv <- function(inv) xInv <<- inv
        ## the following function gets the inverse from the cach
        getInv <- function() xInv
        ## return as the list the above functions
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}
## cacheSolve function calculates the inverse of the special "matrix" created with the makeCacheMatrix.
## cacheSolve first checks to see if the inverse of the matrix has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.
cacheSolve <- function(x, ...) {
        ## 
        ## Check if the inverse is already calculated
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## get the data from the special "matrix"
        data <- x$get()
        ## calculate its inverse
        inv <- solve(data, ...)
        ## store the inverse in the cach so that the next time it wont need to be calculated again
        x$setInv(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}
