## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## x is a square invertible matrix
## returns a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## x is the output of makeCacheMatrix()
  ## returns inverse of the original matrix input to makeCacheMatrix()
  inv = x$getinv()
  if (!is.null(inv)){
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  inv
}
