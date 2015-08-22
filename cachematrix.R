## Below are two R-function's that compute and cache the inverse of a matrix 
## so that when we need it again, it can be looked up in the cache rather than 
## recomputed. These functions can be used to create a special object that 
## stores a matrix and cache's its inverse.

## The makeCacheMatrix function creates a list containing a function to
## set the value of a matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## and get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invMat <- NULL
        set <- function(y) {
                x <<- y
                invMat <<- NULL
        }
        get <- function() x
        setInvMat <- function(inverse) invMat <<- inverse
        getInvMat <- function() invMat
        list(set = set, get = get,
             setInvMat = setInvMat,
             getInvMat = getInvMat)

}


## The cacheSolve function computes the inverse of the matrix created with
## the makeCacheMatrix function. However, it first checks to see if the inverse 
## matrix has already been computed. If so, it gets the inverse matrix from the
## cache and skips the computation. Otherwise, it computes the inverse of the 
## matrix and sets the value of the inverse matrix in the cache via the 
## setInvMat function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMat <- x$getInvMat()
        if(!is.null(invMat)) {
                message("getting cached data")
                return(invMat)
        }
        data <- x$get()
        invMat <- solve(data, ...)
        x$setInvMat(invMat)
        invMat
}
