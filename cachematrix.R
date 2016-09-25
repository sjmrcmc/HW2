## makeCacheMatrix and cacheSolve work together to compute and cache the inverse
## of an invetible matrix

## Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
 
  ## Returns a list containing functions to
  ##   1.set the value of the matrix
  ##   2.get the value of the matrix
  ##   3.set the value of the matrix inverse
  ##   4.get the value of the matrix inverse
  
  ## Initiate matrix inverse to NULL
  mInv <- NULL
  
  ## Define the "set" function
  set <- function(y) {
    x <<- y
    mInv <<- NULL
  }
  
  ## Define the "get" function
  get <- function() x
  
  ## Define the "setInv" function to set the matrix inverse
  setInv <- function(solve) mInv <<- solve
  
  ## Define the "getInv" function to get the matrix inverse
  getInv <- function() mInv
  
  ## Return a list with the above 4 elements
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}


## Computes the inverse of the special matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {

  ## If the inverse has already been calculated (and the matrix has not changed), 
  ##   then cachesolve will retrieve the inverse from the cache
  ## If the inverse has not already been calculated,
  ##   then cachesolve will compute and cache the inverse
  
  ## Matrix inverse is computed using the solve function
  
  ## NOTE: cachesolve assumes the matrix is invertible
  
  ## Function argument "x" is a list returned by makeCacheMatrix
  
  ## check whether matrix inverse has previously been cached
  ## if so, return the inverse matrix
  mInv <- x$getInv()
  if(!is.null(mInv)) {
    message("Getting matrix inverse from cache")
    return(mInv)
  }
  
  ## matrix inverse not previously calculated
  ## get data, calculate inverse, cache result and return inverse
  mtrx <- x$get()
  mInv <- solve(mtrx, ...)
  x$setInv(mInv)
  mInv
 
}
