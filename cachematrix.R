## Running some computations is time consuming. This is especially true when you need to reuse
## a value(s) computed by a preceding function in subsequent calculation(s). As such, it is more
## efficient to cache the results and readily use them instead of computing them again. 
## For example, maxtrix inversion is usually costly, especially when running inside of a loop. 
## The following functions compute and cache the inverse of a matrix.

## The first function, makeCacheMatrix(), creates a special "matrix" object that can cache 
## its inverse. This function is a list containing a function to:

##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse
##4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## x: a square invertible matrix (assumption)
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse 
        getinv <- function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This second function, cacheSolve(), computes the inverse of the special "matrix" returned by the 
## makeCacheMatrix() above. If the inverse has already been calculated (and the matrix has not changed), 
## this function retrieves the inverse from the cache directly. It uses the list created by 
## the makeCacheMatrix() as the input.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
        inv <- x$getinv()
        ## checks if the inverse has already been calculated
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        ## otherwise, calculates the inverse
        mat.data <- x$get()
        inv <- solve(mat.data, ...)
                ## sets the value of the inverse in the cache via the setinv function
        x$setinv(inv)
                return(inv)
}
