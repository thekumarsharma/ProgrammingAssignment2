
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data.")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    inverse
}