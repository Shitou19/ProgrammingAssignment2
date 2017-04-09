## Put comments here that give an overall description of what your
## functions do

## A function to create a special matrix that supports caching of the matrix's inverse

makeCacheMatrix <- function(mat = matrix()) {
  cached_inv <- NULL
  set <- function(new_mat) {
    mat  <<- new_mat  # assign a new matrix
    cached_inv <<- NULL  # invalidate chached inverse
  }
  get <- function() mat 
  setinv <- function(inv) cached_inv <<- inv
  getinv <- function() cached_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## A function that returns that inverse of a matrix
## The inverse value is cached so that it is not calculated twice

cacheSolve <- function(special_mat, ...) {
  cached_inv <- special_mat$getinv()
  
  # check if the inverse value is already cached
  if(!is.null(cached_inv)) {
    message("getting cached data")
    return(cached_inv)
  }
  
  # inverse value is not cahced -> calculate it
  mat <- special_mat$get()
  calculated_inv <- solve(mat, ...)
  special_mat$setinv(calculated_inv)
  calculated_inv
}
