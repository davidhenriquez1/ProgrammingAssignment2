## This file contains the two functions makeCacheMatrix and cacheSolve. This pair of
## functions serve to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of four functions which can set the value of the matrix, get the
## value of the matrix, set the value of the inverse, and get the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##This function calculates the inverse of the special "matrix" returned by makeCacheMatrix
## It first checks to see if the inverse has already been calculated. If so, the inverse
## will retrieve the inverse of the cache. If it has not been calculated, it will 
## calculate the inverse using the solve function and save to the cache.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}

