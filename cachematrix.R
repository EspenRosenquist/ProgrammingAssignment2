## Programming assignment 2
## makeCacheMatrix : creates a matrix object able to cache itself and it's inverse
## cacheSolve : checks if the inverse of above matrix is cached and returns said inverse matrix
## assume that input matrix is inversible - i.e. no error handling


## Use:
## testMatrix - a randomly seeded matrix 1000*1000
## set.seed(424242)
## rn <- rnorm(100000)
## aMatrix <- matrix(rn,nrow = 1000, ncol = 1000)

## a test to see if two matrices are equal, by Rui Barradas(06/12/2012)
## from: https://stat.ethz.ch/pipermail/r-help/2012-June/315408.html
## matequal <- function(x, y) {is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)}
## matequal(aMatrix, cacheMatrix$get())

## retrieve inverse of cached matrix:
## reverseMatrix <- cacheSolve(cacheMatrix)


## --- makeCacheMatrix ---
## This function creates a special "matrix" object that can cache its inverse
## it does not actually contain any conversion to inverse matrix, only the pointers to
## cache objects that are defined in the workspace where they are created.
## The special denotation <<- assigns a value/object to a named cache.
## Similar to static variable in some other languages, or class get/set
## a cacheable matrix:
## cacheMatrix <- makeCacheMatrix(aMatrix)

makeCacheMatrix <- function(m = matrix()) {
	## reset inverse matrix if this function is first invoked
	m.inverse <- NULL
	
	## method set matrix, nullifies inverse
	set <- function(m.this) {
		m <<- m.this
		m.inverse <<- NULL
	}
	## method to retrieve matrix - from input
	get <- function() m
	
	## methods to get and set inverse matrices
	## will return NULL if they do not exist
	setInverse <- function(inverse) m.inverse <<- inverse
	getInverse <- function() m.inverse
	
	## function returns a list
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## --- cacheSolve ---
## Given a cached matrix made with makeCacheMatrix, returns inverse matrix
## and
## caches inverse matrix by use of same matrix object.
## If inverse matrix is already cached, retrieves inverse matrix from cache.
## For larger matrices and iterative procedures. Have the potential to save a LOT of time!
## Reversing the argument from if(!is.null(m.inverse)) to if(is.null(m.inverse)) is natural
## as most likely case is that cache will be empty first time around.
## Notice that returned value is at the very end of function:
## - either reverse matrix has been created or it already existed in cache
## On tests with randomly filled 1000 x 1000 matrix, this method runs very much faster than !is.null

cacheSolve <- function(m, ...) {
	## retrieve inverse matrix from cache
	m.inverse <- m$getInverse()
	
	## assume cache is empty, which is probably true the first time you try the cache.
	if(is.null(m.inverse)) {
		message("Caching given matrix")
		m.norm <- m$get()
		message("Caching inverse matrix")
		## do the actual inversion
		m.inverse <- solve(m.norm, ...)
		m$setInverse(m.inverse)
	} else {	
		## if cache is not empty, do nothing
		message("Retrieving cached inverse matrix")
	}
	## returns the inverse matrix
	m.inverse
}
