## The functions below creates a matrix object and caches its inverse and then return the matrix inverse.

## makeCacheMatrix is a function that creates a list of functions that will be 
## used in the cacheSolve function.  
## set: sets the matrix
## get: gets the matrix
## setinverse: sets the inverse of the matrix
## getinverse: gets the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve either calculates and returns the inverse of a matrix or if the 
## inverse of that matrix has been previously calculated and cached, cacheSolve 
## returns the inverse from cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}



