## These functions are used together to calculate the inverse of a matrix
## and store that result on the cache memory in order to save time consuming computing 

## The first function stores a list of functions that are later used by the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(solve) inv <<- solve
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}	


## Checks if the inverse of this matrix has already been calculated and cached
## If so, it gets the information from cache. If not, it calculates and stores in
## the inverse in the cache

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)	
	inv
	## Return a matrix that is the inverse of 'x'
}
