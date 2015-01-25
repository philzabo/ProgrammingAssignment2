## These functions cache and compute the inverse of a matrix.

## The first function makes a matrix object that caches its own inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The second function calculates the inverse of the matrix object returned
## by makeCacheMatrix in the first fucntion. If the inverse has already
## been calculated, then cacheSolve gets the inverse from the previously
## created cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {message("Getting cached data...")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
