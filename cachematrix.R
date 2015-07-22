## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y;
                i <<- NULL; #it also initiates inv to null
        }
        get <- function() return(x); # return the matrix
        setinv <- function(inv) i <<- inv; # set the inversed matrix
        getinv <- function() return(i); # return the inversed matrix
        return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix above.If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        # get the inversed matrix
        i <- x$getinv()
        if(!is.null(i)) { # if the inversion result is there
                message("getting cached data...")
                return(i) # return the calculation
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        return(i)
}
