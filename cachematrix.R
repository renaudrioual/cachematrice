## code pour la cachematrice et la cachesolve !! 


##  This function sets a matrix in the enviroment, then when the inverse is called, caches the result
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL  ## could not find m unless was initialized to NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	## switched mean for inverse from the vector function
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m

	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}




## fonction de check
## This functions check if the cached result (the matrix inverse) is already stored, if not null, then returns the cached value
##    If the value is not cached, then inverse the cached original matrix (using the solve()) then set the cached value for retrival later

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
	 
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}

