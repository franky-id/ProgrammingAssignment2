## The functions below supports the caching of the matrix inverse

## Set the matrix, get the matrix, set the inverse of the matrix,
## and get the inverse
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set	 <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## Given a special matrix 'x', return its inverse from cached matrix
## , or calculate and cache the inverse (if cache not exist) 
## before returning it
cacheSolve <- function(x, ...) {
		i <- x$getInverse
		if (!is.null(i)){
			return(i)
		}
		data <- x$get()
		i <- solve(data,...)
		x$setInverse(i)
        
		## Return a matrix that is the inverse of 'x'
		i
}
