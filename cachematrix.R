## The functions below allow to calculate the 
## inverse of a given matrix and then cache it
## to avoid repeated computations

## The function creates a special object that
## allows to store an input matrix alongside
## with its inverse
## Input: x - a matrix
## Output: a caching object

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverted) inv <<- inverted
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function either calculates the inverse 
## of the matrix stored in the object returned 
## by the makeCacheMatrix function (if the
## inverse has not been computed yet), or 
## returns the inverse from the cache
## Input: x - a caching object returned by 
##   the makeCacheMatrix function
## Output: the inverse

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
