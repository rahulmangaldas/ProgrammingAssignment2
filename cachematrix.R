## Put comments here that give an overall description of what your
## functions do

## creates a cacheMatrix object
## usage: m <- makeCacheMatrix(); m$set(data)

makeCacheMatrix <- function(m = matrix()) {

        inv <- NULL
        set <- function(x) {
                m <<- x
                inv <<- NULL
        }
        get <- function() m
        setinv <- function(x) inv <<- x
        getinv <- function() inv
        return(list(set = set, get = get, setinv = setinv, getinv = getinv))

}

## if it exists, returns the cached inverse of a cacheMatrix
## else computes and caches it

cacheSolve <- function(cacheMatrix, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- cacheMatrix$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        m <- cacheMatrix$get()
        inv <- solve(m, ...)
        cacheMatrix$setinv(inv)
        return(inv)

}
