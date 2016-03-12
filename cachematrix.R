## makeCacheMatrix will create an invertible matrix. cacheSolve will retrieve the inverse of what is returned by makeCacheMatrix.

## This function creates a special "matrix" object that can cache its ## inverse.

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
set <- function(y) {
	x <<- y
	i <<- NULL
}
get <- function() x
setinverse <- function(inv) i <<- inv
getinverse <- function() i
list(
set + set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve will retrieve the inverse of the special "matrix" that is returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
i <- x$getinverse()
if(!is.null(i)) {
	message("getting cached data")
	return(i)
}       
m <- x$get()
i <- solve(m, ...)
x$setinverse(i)
i
}
