## Matrix inversion is usually a costly computation
## and there may be some benefit to caching the
## inverse of a matrix rather than computing it
## repeatedly. These two functions cache the inverse
## of a matrix.

## This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
			get <- function() x
            getInverse <- function() {
			  m
			}
			setInverse <- function(i) {
				m <<- i
			}
            
            list(get = get, getInverse = getInverse, setInverse = setInverse)
}


## This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above or its
## cached version
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		    m <- x$getInverse()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            m <- x$setInverse(solve(x$get()))
			m
}
