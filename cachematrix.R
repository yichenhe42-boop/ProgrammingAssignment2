## Put comments here that give an overall description of what your
## functions do
## Overall description:
## These two functions work together to cache the inverse of a matrix.
## makeCacheMatrix creates a special "matrix" object that stores a matrix
## and can cache its inverse. cacheSolve computes the inverse of that
## special object, retrieving the cached value if it has already been
## calculated and the matrix has not changed.

## Write a short comment describing this function:
## makeCacheMatrix creates a list of four functions to set/get the matrix
## and set/get its cached inverse. The inverse is initially NULL.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
## Write a short comment describing this function
## Creates a special "matrix" object that can cache its inverse. 
## It contains four functions: set to set the matrix value, get to retrieve it, 
## setinv to store the computed inverse, and getinv to retrieve the cached inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
