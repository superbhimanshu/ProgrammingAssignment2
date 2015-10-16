##### makeCacheMatrix function returns a list of functions for:
# 1. setting the matrix
# 2. getting the matrix
# 3. setting the inverse of matrix
# 4. getting the inverse of matrix


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


##### cacheSolve function does the following
# 1. Computes inverse of the matrix created by makeCacheMatrix function if it doesn't exist in the cache already
# 2. It returns the inverse of matrix from the cache if it already exists 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #create list of functions using makeCacheMatrix
        listFuncs <- makeCacheMatrix(x)
        listFuncs$set(x)
        # check if inverse already exists in cache, if it does(not null) return it 
        inverseMatrix <- listFuncs$getInverse()
        if(!is.null(inverseMatrix)) {
        	message("Inverse exists in cache")
        	return(inverseMatrix)
        }
        #if it doesn't exist find inverse matrix and set the result to cache 
        message("Inverse doesn't exist in cache. Estimating inverse and storing it in cache.")
        inverseMatrix <- solve(x)
        listFuncs$setInverse(inverseMatrix)
        inverseMatrix
}
