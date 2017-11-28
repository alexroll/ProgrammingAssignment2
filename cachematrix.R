## Put comments here that give an overall description of what your
## functions do

## Matrix inversion is highly computed cost consuming.
## We want to cache the inverse of a Matrix rather than compute it.

## Write a short comment describing this function

##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

##set the matrix
##get the matrix
##set the inverse of the matrix
##get the inverse of the matrix


## Example Assignment
##makeVector <- function(x = numeric()) {
##        m <- NULL
##        set <- function(y) {
##                x <<- y
##                m <<- NULL
##        }
##        get <- function() x
##        setmean <- function(mean) m <<- mean
##        getmean <- function() m
##        list(set = set, get = get,
##             setmean = setmean,
##             getmean = getmean)
##}

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

## cacheSolve will return the inverse of matrix except and cache it.
## if the function is used again on a same matrix, the pre computed result is
## shown with an information message.


## Example Assignment
##cachemean <- function(x, ...) {
##        m <- x$getmean()
##        if(!is.null(m)) {
##                message("getting cached data")
##                return(m)
##        }
##        data <- x$get()
##        m <- mean(data, ...)
##        x$setmean(m)
##        m
##}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <-x$getinverse()
        if(!is.null(inv)){
                message("Matrix Pre Computed. Cached Matrix is shown")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}