## Contains 2 functions supporting:
## 1. makeCacheMatrix: Create a special matrix object that cache its inverse
## 2. cacheSolve: Calculate inverse matrix, cache it (if inverse matrix does 
##    exist in the cache) and return

## Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) i <<- Inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Get the matrix object returned by makeCacheMatrix as input parameter,
## get the inverse matrix from the input parameter's cache and return it.
## In case there is no inverse matrix in the cached, this function will calculate
## the inverse matrix and return it.
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
            message("getting cached data")
            return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i ## Return matrix i that is the inverse of 'x'
}