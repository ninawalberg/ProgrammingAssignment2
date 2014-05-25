## Programming assignement in the R programming Coursera course
## File contains functions to cre<<ate and save the inverse of a matrix to cache 

## This function creates a special "matrix" object that can cache its inverse.

## Created by Nina Walberg 24.05.2014

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  ## Assigne the value of the inverse to the cache
  setinverse <- function(inverse) m <<- inverse
  ## return cached inverse result
  getinverse <- function() m
  ## The function return a list containing the function elements to save the
  ## cached matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Get the content of getinverse from the special object x
  m <- x$getinverse()
  ## test if there has already been calculated the inverse of the matrix
  ## If the inverse already exist, the inverse is returned, and the function ends
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if the inverse didn't exist, it has to be calculated a stored in cache
  ## get the matrix
  data <- x$get()
  ## create the inverse of the matrix
  m <- solve(data,...)
  ## Save the inverse to cache
  x$setinverse(m)
  ## return the inverse of the matrix
  m
}
