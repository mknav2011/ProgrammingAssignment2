## Put comments here that give an overall description of what your
## functions do

## function to cache inverse of a matrix
## Example:
## source("cacheMatrix.R")
## mk <- makeCacheMatrix(matrix(c(10,20,30,40),nrow=2,ncol=2))
## cacheSolve(mk)
##      [,1]  [,2]
## [1,] -0.2  0.15
## [2,]  0.1 -0.05

makeCacheMatrix <- function(x = matrix()) {
		itemp <- NULL
		
		set <- function(y){
			x <<- y
			itemp <<- NULL
		}
		
		get <- function() x
		
		setinverse <- function(inv) itemp <<- inv
		
		getinverse <- function() itemp
		
		list(
			set = set,
			get = get,
			setinverse = setinverse,
			getinverse = getinverse
		)
}


## To get the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        
        if (!is.null(i)){
        	message("getting cached matrix")
        	return(i)
        }
        m <- x$get()
        i <- solve(m, ...)
        x$setinverse(i)
        i
}

