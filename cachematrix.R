## The following code makes use of caching to store the inverse of
## Matrix (if calculated already) in order to avoid the repeat computation , 
## since Matrix inversion is a costly computation

## The makeCacheMatrix creates a special 'matrix' object
## that can cache its inverse. It consists of the following functions
## set -> set the value of Matrix
## get -> get the value of Matrix
## setinverse -> set the inverse of the Matrix
## getinverse -> get the inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(y) {
            x <<- y
            inverseMatrix <<- NULL
        }
        get <- function() x
        setinverse <- function(iMatrix) inverseMatrix <<- iMatrix
        getinverse <- function() inverseMatrix
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## The cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache. It uses
## solve function in R to calculate the inverse of the matrix

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        ## Return a matrix that is the inverse of 'x'
        x$setinverse(m)
        m
}
