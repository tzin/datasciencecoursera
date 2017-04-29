## The first function creates an object that can allow users to cache its inverse. 
## The second function allows users to compute the inverse more quickly, by retrieving 
## the inverse from the cache if it has already been calculated. If not, it will 
## still caculate the inverse.

## Creates a matrix object that can cache its inverse by doing the following steps:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
## Given that matrix is invertible. No error handling for non invertible matrices

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinvrs <- function(solve) invrs <<- solve
  getinvrs <- function() invrs
  list(set = set, get = get, setinvrs = setinvrs, getinvrs = getinvrs)
  
}


## computes the inverse of special matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated, then, cacheSolveshould retrieve the inverse from cache.
## Given that matrix is invertible. No error handling for non invertible matrices

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invrs <- x$getinvrs()
  if(!is.null(invrs)){
    message("getting cached data")
    return(invrs)      
  }
  ## Calculate new inverse as no inverse is calculated
  data <- x$get()
  invrs <- solve(data)
  x$setinvrs(invrs)
  invrs 
}