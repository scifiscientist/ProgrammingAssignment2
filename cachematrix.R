## Below are two R-function's that compute and cache the inverse of a matrix 
## so that when we need it again, it can be looked up in the cache rather than 
## recomputed. These functions can be used to create a special object that 
## stores a matrix and cache's its inverse.

## The makeCacheMatrix function creates a list containing a function to
## set the value of a matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## and get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
        invMat <- NULL
        
        ##sets the values in the matrix and resets the inverse matrix object
        ##to NULL
        set <- function(y) 
                {
                        x <<- y
                        invMat <<- NULL
                }
        
        
        ##gets(fetches) the matrix
        get <- function() x
        
        ##sets the values in the inverse matrix object based on its computation
        ##in the CacheSolve function
        setInvMat <- function(inverse) invMat <<- inverse
        
        ##gets(fetches) the inverse matrix
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

cacheSolve <- function(x, ...) 
{
        ## Returns a matrix that is the inverse of 'x' by calling the 
        ## getInvMat() function of the makeCacheMatrix() function
        invMat <- x$getInvMat()

        ## If cached data is present, returns the same        
        if(!is.null(invMat)) 
        {
                message("getting cached data")
                return(invMat)
        }
        
        ## if cached data is not present, fetches the matrix values by calling 
        ## the get() function of the makeCacheMatrix() function
        data <- x$get()
        
        ## computes the values in the inverse matrix using the solve() function
        invMat <- solve(data, ...)
        
        ## stores the computed inverse matrix values in cache using the 
        ## setInvMat() function of the makeCacheMatrix() function
        x$setInvMat(invMat)
        
        ## returns the inverse matrix values 
        invMat
}
