## LF 2016-10-27
## The idea of this assigment is to learn how
## to cache potentially expensive operations
## An example is given about how to cache
## the mean of a vector and I use the same
## code but changing the "mean" function
## by the "solve" function which will
## invert a matrix instead.
## Please not that the determinant cannot
## be zero for us to be able to invert a
## matrix

## This function creates a special 
## matrix object with the cache
## the idea is that we will only
## compute the inverse once.
## If it has already been computed
## then we only return the value
## Note that when the matrix is set
## we have to empty the cache so that
## it is recalculated again.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## This function behaves as a simple cache
## if the function has already been called
## then we return the cached data
## otherwise we will use the solve function
## to invert the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}