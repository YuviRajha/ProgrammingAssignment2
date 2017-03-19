## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly 
## Below is a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.

## cacheSolve is a function that computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 

## The first function, makeCacheMatrix creates a special "matrix" containing a function to:

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of a square matrix
## get the value of the inverse of a square matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y = matrix()) {
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

## Then cacheSolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix will be done with the solve function in R. 
## We assume that X is a square invertible matrix, and so solve(X) will return its inverse.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
                m
        }
                y <- x$get()
                m <- solve(y, ...)
                x$setsolve(m)
                m
}
