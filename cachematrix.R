## The following are the functions that make an inverse matrix from a given one
## and try to retrieve from cache if already computed.

## This function tries to set a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list( set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function tries to retrieve cached inversed matrix if available
## it will compute a new one if there is no cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}

	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
