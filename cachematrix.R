## There are two functions ( makeCacheMatrix and cacheSolve)
## The makeCacheMatrix creates a special "matrix" object. It takes a matrix
## as input and returns a "matrix object which contains the matrix and
## caches it's inverse.
## The cacheSolve is used to compute the inverse of the matrix. It takes
## the "matrix" object as input from makeCacheMatrix() and checks if inverse
## is present. If it is, then it gets inverse from the cache else, it
## calculates the inverse and sets value in cache.


## This function creates a "matrix" object which contains the functions:
## set(), get(), setinverse(), getInverse()

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set=set, get=get, setinverse=setinverse, getInverse=getInverse)
}


## This function computes inverse of the "matrix" object from the previous function
## it is stored in m and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- inverse(data, ...)
        x$setinverse(m)
        m
}
