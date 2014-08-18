## 
## These functions work in tandem to return the inverse of a matrix "x".
##
## The cover function checks to see if the inverse has already been cached
## before calling solve(x) to do the work. The cpu savings can be 
## significant for very large matrices where the inverse is used repeatedly
## in various functions.
##
## Example Usage:
##
##      # Setup
##		x = matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE)
##		a = makeCacheMatrix(x)
##
##		# Invoke (first time calculated, second time retrieved from cache)
##		cacheSolve(a)
##			Calculating inverse...
##		cacheSolve(a)
##			Getting cached data...			
##
########################################################################

##
## Find the inverse of a matrix "x" and cache the result.
## This function returns a list of 4 possible operations on "x". 
##     set: set the data matrix (ie, initialize it)
##     get: get the data matrix 
##     setsolve: set and cache the inverse matrix
##     getsolve: retrieve the inverse matrix
##
makeCacheMatrix <- function(x = matrix()) {

		# set data matrix
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

		# get data matrix
        get <- function() x

		# set and cache the inverse matrix 
        setsolve <- function(solve) m <<- solve 

		# retrieve the input matrix
        getsolve <- function() m

		# return operations vector
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}

##
## This is the cover function for returning the inverse of a matrix "x".
##
cacheSolve <- function(x, ...) {

		# check if we have the inverse cached, return it and exit. 
        m <- x$getsolve()
        if(!is.null(m)) {
                message("  Getting cached data...")
                return(m)
		}

		# otherwise we calculate it calling solve().
        message("  Calculating inverse...")
        data <- x$get()
        m <- solve(data, ...)

		# cache it before exiting.
        x$setsolve(m)
        m
}
