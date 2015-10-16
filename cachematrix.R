##Author: Himanshu Verma
## October, 15 2015
##### makeCacheMatrix function returns a list of functions for:
# 1. setting the matrix
# 2. getting the matrix
# 3. setting the inverse of matrix
# 4. getting the inverse of matrix


## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

	#we need to set a variable (similar to static which remains in environment, once initialized)
	listFunc <<- attr(makeCacheMatrix, "staticObjFuncs")
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

	if(is.null(listFunc)) {
		listFunc <- list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
	}

	attr(makeCacheMatrix, "staticObjFuncs") <<- listFunc

	listFunc

	
}


##### cacheSolve function does the following
# 1. Computes inverse of the matrix created by makeCacheMatrix function if it doesn't exist in the cache already
# 2. It returns the inverse of matrix from the cache if it already exists 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	    listFunc <- makeCacheMatrix(x)

	    #check if the existing matrix in cache is equivalent to the matrix passed here or not
	    #if they are not equal clear the cache and set new matrix value in the cache
    	oldMatrix = listFunc$get()
    	if(dim(x)!=dim(oldMatrix) || !all(x==oldMatrix)) {
        	listFunc$setInverse(NULL)
        	listFunc$set(x)
    	} 
        # check if inverse already exists in cache, if it does(not null) return it 
        inverseMatrix <- listFunc$getInverse()
        if(!is.null(inverseMatrix)) {
        	message("Inverse exists in cache")
        	return(inverseMatrix)
        }
        #if it doesn't exist find inverse matrix and set the result to cache 
        message("Inverse doesn't exist in cache. Estimating inverse and storing it in cache.")
        inverseMatrix <- solve(x)
        listFunc$setInverse(inverseMatrix)
        inverseMatrix
}
