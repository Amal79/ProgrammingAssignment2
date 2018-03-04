#The first function, makeCacheMatrix creates a special creates a special "vector", which is really a list containing a function to
#1.set the value of the matrix
#2.get the value of the matrix
#3.set the value of the inverse of the matrix
#4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  matrix_inverse <- NULL 
  set <- function(y) { 
    x <<- y 
    matrix_inverse <<- NULL 
  } 
  get <- function() x 
  setinverse <- function(solve) matrix_inverse <<- solve 
  getinverse <- function() matrix_inverse 
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function return the inverse of the matrix
#first, it checks if the inverse matrix has already been calculated. 
#If so, it gets the inverse matrixfrom the cache and skips the computation. 
#Otherwise, it calculates the inverse matrix and sets the value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  matrix_inverse <- x$getinverse() 
  if(!is.null(matrix_inverse)) { 
    message("getting cached data") 
    return(matrix_inverse) 
  } 
  data <- x$get() 
  matrix_inverse <- solve(data) 
  x$setinverse(matrix_inverse) 
  matrix_inverse 
}
