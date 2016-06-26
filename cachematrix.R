## The R file contains functions to create a cache for the inverse of an matrix
## and also means of retrieving it

## 			makeCacheMatrix() 
## Will  create a list of functions that will facilitate
## the maintaing of a matrix and its inverse matrix.If the matrix is changed 
## then the cache is emptied to clear the old data
## The function returns a list of functions:
## 1.set() : set input matrix
## 2.get() : get input matrix
## 3.setInverseMatrix(): set inverse matrix
## 4.getInverseMatrix(): get inverse matrix
## 5.isequal(): check if 2 matrices are equal

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setData <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getData <- function() x
  setInverseMatrix <- function(data) inverse <<- data
  getInverseMatrix <- function() inverse
  isEqual<- function(x, y)	is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
  list(set = setData, get = getData,
       setInverseMatrix = setInverseMatrix ,
       getInverseMatrix = getInverseMatrix,
       isEqual = isEqual )
  
}

## 			cacheSolve() 
## This functions get the inverse from the cache if present or else 
## creates a new inverse and stores in cache
## The function returns the inverse of a matrix

cacheSolve <- function(x, ...) {
  m <- x$getInverseMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInverseMatrix(m)
  m
}
