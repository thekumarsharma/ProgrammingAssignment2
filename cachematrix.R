#Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly. Following are the pair of functions that cache the inverse of a matrix.

# makeCacheMatrix function creates a list containig function
# 1. set -> set the value of the matrix, NULL to inverse 
# 2. get -> get the value of the matrix
# 3. setInverse -> set the value of inverse of the matrix
# 4. getInverse -> get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }	
    get <- function(){
		x
	}	
    setInverse <- function(inv){
		inverse <<- inv
	}	
    getInverse <- function(){
		inverse
	}
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse with solve function, sets the value in the cache via
# setInverse function and return the value.

# This function assumes that the matrix is always invertible.
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


# Sample Input and their output after calling respective functions
# > x = rbind(c(1, 2), c(2, 1))
# > m = makeCacheMatrix(x)
# > m$get()
#     [,1] [,2]
# [1,]    1    2
# [2,]    2    1

# No cache in the first run
# > cacheSolve(m)
#           [,1]       [,2]
# [1,] -0.3333333  0.6666667
# [2,]  0.6666667 -0.3333333


# Retrieving from the cache in the second run
# > cacheSolve(m)
# getting cached data.
#           [,1]      [,2]
# [1,] -0.3333333  0.6666667
# [2,]  0.6666667 -0.3333333
