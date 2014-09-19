## This file contains two functions which together either 
## solve the inverse of a matrix, or retrieve a cached solution. 

## This first fuction sets up the tools to retrieve and store
## the results of the inverse operation.  x represents the original 
## matrix, and m is the inverse of the matrix.  They are defined in 
## the global environment, so that the other function will be able to
## reference them.  This function will "get" or retrieve the values
## and "set" or store the values.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)    
}


## This function will first check to see if an inverse has already been
## calculated for a particular matrix.  If it has been calculated and 
## stored, it will display the cached value.  If it has not, then it will 
## calculate the inverse of the matrix, display it, and store it in 
## memory for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}