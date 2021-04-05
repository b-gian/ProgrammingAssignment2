## Put comments here that give an overall description of what your
## functions do

## This function creates a Cache matrix to compute 
## its inversion faster.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinv <- function(solve) m <<- solve
	getinv <- function() m
	list(set = set, get = get, setinv = setinv,
		getinv = getinv)
}


## This function returns the inverse of the matrix,
## if it already exists from the previous function, 
## or calculates it again, in this new function environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if (!is.null(m)) {
        	message('getting cached data')
        	return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
        
}
