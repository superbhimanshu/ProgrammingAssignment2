## Put comments here that give an overall description of what your
##### makeCacheMatrix function returns a list of functions for:
# 1. setting the matrix
# 2. getting the matrix
# 3. setting the inverse of matrix
# 4. getting the inverse of matrix

##### cacheSolve function does the following
# 1. Computes inverse of the matrix created by makeCacheMatrix function if it doesn't exist in the cache already
# 2. It returns the inverse of matrix from the cache if it already exists 
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

	inverseMatrix <- NULL
	set <- function(y) {
		x <<- y
		inverseMatrix <<- NULL
	}

	get <- function() {
		x
	}

	setInverse <- function(inv) {
		inverseMatrix <<- inv
	}

	getInverse <- function(){
		inverseMatrix
	}

	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
