## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat = matrix()) {
  cached_inv <- NULL
  set <- function(new_mat) {
    mat  <<- new_mat
    cached_inv <<- NULL
  }
  get <- function() mat 
  setinv <- function(inv) cached_inv <<- inv
  getinv <- function() cached_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(special_mat, ...) {
        ## Return a matrix that is the inverse of 'special_mat'
  cached_inv <- special_mat$getinv()
  if(!is.null(cached_inv)) {
    message("getting cached data")
    return(cached_inv)
  }
  mat <- special_mat$get()
  calculated_inv <- solve(mat, ...)
  special_mat$setinv(calculated_inv)
  calculated_inv
}
