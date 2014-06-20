## Matrix inversion can be computationally intense.
## The two functions below work together
## with an invertible matrix to make a list object
## that contains the calculated the inverse matrix,
## and makes it available for future retrieval.


## makeCacheMatrix creates a list object for matrix x,
## where x is either passed as an argument
## or set by the set() function.
## makeCacheMatrix can set/return x
## and save/return inverse matrix of x.

makeCacheMatrix <- function(x = matrix()) {
	
	# Variable  x_inv: Inverse matrix of x	
	x_inv <- NULL
	

	# Function  set: Set x and reset cached inverse	
	set <- function(y = matrix()){
		x <<- y
		x_inv <<- NULL
	}
	

	# Function  get
	get <- function() x


	# Function  setInverse: Cache the inverse matrix of x
	setInverse <- function(inv) x_inv <<- inv


	# Function  getInverse: Fetch the cached inverse matrix of x	
	getInverse <- function() x_inv


	# Return list object to allow set/get x and set/get inverse of x
	list(set = set, get = get,
		setInverse = setInverse, getInverse = getInverse)

}


## cacheSolve takes a makeCacheMatrix object and
## returns the inverse matrix.
## The function returns a cached inverse matrix if it exists.
## Otherwise, the function uses solve() to find the inverse
## then stores it in the makeCacheMatrix object. 

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of x
	
	# Fetch the cached inverse of x
	inv <- x$getInverse()

	
	# If the inverse of x was previously calculated and cached,
	# return it (and exit cacheSolve).
	if(!is.null(inv)){
		message('Getting cached inverse matrix...')
		return(inv)		
	}


	# Calculate the inverse of x
	inv <- solve(x$get(), ...)


	# Cache the inverse
	x$setInverse(inv)


	# Return the inverse
	inv
	
}
