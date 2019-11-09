## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special matrix which is really a mtarix consisting of a function to 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv<<- inverse
	getInverse <- function() inv
	list(set=set, get=get, setInverse = setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
#The following function calculates the inverse of the special matrix created with the above function. However, it first checks to see if the inverse has already been calculated. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function. 
cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv  <- solve(data, ...)
	x$setInverse(inv)
	inv
        ## Return a matrix that is the inverse of 'x'
}
