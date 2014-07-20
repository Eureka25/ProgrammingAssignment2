## These functions enable the caching of a matrix's 
## inverse to prevent costly recalculation. It assumes
## that the matrices passed in are invertible so no
## error-checking is done.


## makeCacheMatrix - create a matrix object that is 
## able to cache its inverse.
## It has getter and setter methods for the matrix and
## the inverse.

## Input  : matrix 'x'
## Output : cache matrix

makeCacheMatrix <- function(x = matrix()) {

    #initialise the inverse to be NULL so that we can
    #check if we have calculate it or not.
    inv <- NULL

    #set the matrix and reset the inverse to be NULL
    #since the matrix may have changed
    set <- function(y) { 
    	   	       	 x <<- y
			 inv <<- NULL
		       }

    get <- function() x

    setInv <- function(solve) inv <<- solve

    getInv <- function() inv

    list(set = set, get = get,
         setInv = setInv,
	 getInv = getInv) 
}


## This function uses a cached matrix object created by 
## the above function to get the inverse of a matrix, 
## either by calculating it or retrieving it from a cached
## value if it has already been calculated.

## Input  : cache matrix 'x'
## Output : inverse of x (plus will cache value for later access)

cacheSolve <- function(x, ...) {
	
	inv <- x$getInv()

	#if inv is not NULL, we are using the cachedvalue
	if (!is.null(inv)) {
	    message("Retrieving cached data.")
	    return(inv)
	}

	#if inv is NULL, the inverse has not been calculated
	#so should be (using 'solve') and then stored in the
	#makeCacheMatrix object. Finally we return the inverse.
        matrix <- x$get()
	inv <- solve(matrix)
	x$setInv(inv)
	inv
}
