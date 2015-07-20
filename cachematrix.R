## The pair of functions create a matrix object that caches its inverse, so calculation
## time is saved for repeated inverse retrival.
## makeCacheMatrix() creates such a matrix object; 
## cacheSolve() returns an inverse of the matrix object, possibly a cached result.
## By CKL

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # sets the original matrix and clears the inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # returns the original matrix
    get <- function() x
    
    # sets the new inverse matrix
    setInv <- function(newInv) inv <<- newInv
    
    # returns the inverse matrix
    getInv <- function() inv
    
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## This function returns the inverse of the special matrix x made by makeCacheMatrix(). 
## If a cached invserse is available, the cached inverse will be returned, otherwise an inverse
## will be calcuated, returned, and stored as cache for subsequent readings

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv) #return the cache if available
    }
    #otherwise calculate a new one and cache it
    srcMat <- x$get()
    inv <- solve(srcMat, ...)
    x$setInv(inv)
    return(inv)
}
