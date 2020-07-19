## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
	inv <- NULL                                         #assigning null to variable inv
	set <- function(y){                                 #setting value of matrix
		x <<- y
		inv <<- NULL
	}
	get <- function() {x}
	setInverse <- function(inverse) {inv <<- inverse}  #setting the value of the inverse
	getInverse <- function() {inv}                     #get the value of the inverse function
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {                 
	inv <- x$getInverse()              #writes a matrix that is inverse of x and assigns it to inv
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setInverse(inv)
	inv
}

