## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL  # inv_matrix will contain value of inverted x
  set <- function(new_matrix) { # set function can give a new matrix value to x
    x <<- new_matrix
    inv_matrix <<- NULL # After new value is given to x, inv_matrix should be NULL
  }
  get <- function() x # get function will bring a value of x
  setinverse <- function(inverse) inv_matrix <<- inverse # setinverse function can change value of inv_matrix
  getinverse <- function() inv_matrix # getinverse function will bring a value of inv_matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getinverse() # Bring value of inv_matrix 
  if(!is.null(inv_matrix)) {
    message("getting cached data") # If inv_matrix contain certain value, cached data will be used
    return(inv_matrix)
  }
  matrix_for_inverting <- x$get() # x will be the matrix for inverting
  inv_matrix <- solve(matrix_for_inverting, ...) # If inv_matrix is NULL, solve function will inverse x
  x$setinverse(inv_matrix) # Calculated inv_matrix is put into inv_matrix
  inv_matrix
}
