## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix will  create a matrix and make provision for storing the inverse
## of the matrix in the cache. If the matrix is changed then the cache is emptied to
## make space for new inverse data

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function(inverse) m
	isequal<- function(x, y)	is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse,
	     isequal = isequal )

}


## This functions get the inverse from the cache is present or else creates a new inverse and
## stores in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse(x)
        if(!is.null(m)) {
            message("getting cached data")
		return(m)
        }
        data <- x$get()
	m <- solve(data,...)
        x$setinverse(m)
        m
}
