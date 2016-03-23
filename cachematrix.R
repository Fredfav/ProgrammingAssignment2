## Put comments here that give an overall description of what your
## functions do

## Example output: 
##>Test_Matrix <-makeCacheMatrix(matrix(c(2,0,0,1), c(2,2)))
##> cacheSolve(Test_Matrix)
##  [,1] [,2]
##  [1,]  0.5    0
##  [2,]  0.0    1
##> cacheSolve(Test_Matrix)
##  getting cached data
##  [,1] [,2]
##  [1,]  0.5    0
##  [2,]  0.0    1

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inverse
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inversce of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrice <- x$get()
  inverse <- solve(matrice, ...)
  x$setinverse(inverse)
  inverse
}