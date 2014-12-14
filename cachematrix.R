## Put comments here that give an overall description of what your
## functions do

## Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInversion <- function(inversion) i <<- inversion
    getInversion <- function() i
    list(set = set, get = get,
         setInversion = setInversion,
         getInversion = getInversion)
}


## Get the matrix object returned by makeCacheMatrix as input parameter,
## get the inverse matrix from the input parameter's cache and return it.
## In case there is no inverse matrix in the cached, this function will calculate
## the inverse matrix and return it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInversion()
    if(!is.null(i)) {
            message("getting cached data")
            return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInversion(i)
    i
}