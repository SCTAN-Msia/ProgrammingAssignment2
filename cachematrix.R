## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of 
## a matrix rather than compute it repeatedly.

## The following code is a pair of function use to compute 
## the inverse of the special "matrix" returned by makeCacheMatrix.

## (1) makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.

## "inverse()" is standard R function to calculate the inverse function of 
## a cumulative distribution function.

makeCacheMatrix <- function(x = matrix()) {
        
        invMatrix <- NULL
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invMatrix <<- inverse
        getInverse <- function() invMatrix
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)       
        
}


## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix as per (1) above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getsolve()
        if(!is.null(invMatrix)) {
                message("getting inversed matrix")
                return(invMatrix)
        }
        OriMatrix <- x$get()
        invMatrix <- solve(OriMatrix, ...)
        x$setsolve(invMatrix)
        invMatrix
}
