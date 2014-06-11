
## makeCacheMatrix creates a special "matrix", that is really a 
## list containing a function to
* set the value of the matrix
* get the value of the matrix
* set the value of the inverse of the matrix
* get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  local_inv <- NULL
  ## sets the matrix value 
  set <- function(y){
    x <<- y
    local_inv <<- NULL
  }
  ## retrieves the matrix value
  get <- function() x
  ## sets the inverse value of the matrix
  setInverse <- function(inverse) local_inv <<- inverse
  ## retrieves the inverse value of the matrix
  getInverse <- function() local_inv
  ##list of the functions inside makeCacheMatrix
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}



## cacheSolve function calculates the inverse of the special "matrix"
## that was created by makeCacheMatrix above. cacheSolve function first
## checks if the inverse of the matrix has already been calculated. If
## so, it returns the inverse of the matrix from cache and skips the
## computation; otherwise cacheSolve function calculates the inverse
## of the matrix of the data and stores it in the cache via setInverse
## function

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  ## Retrieve the cached value of inverse      
  local_inv <- x$getInverse()
  ##if inverse exists, return the cached value of inverse
  if(!is.null (local_inv)){
    message ("getting cached Inverse of Matrix")
    return (local_inv)
  }
  #retrieve the special matrix data that was created by "makeCacheMatrix"
  data <- x$get()
  ## calculates the inverse of the matrix
  local_inv <-solve(data, ...)
  ## sets the inverse of matrix in cache
  x$setInverse(local_inv)
  local_inv
  
}

