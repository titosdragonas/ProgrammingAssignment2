## makeCacheMatrix returns a wrapper object (list of functions and their context) around the supplied matrix that is able to cache its inverse when using the cacheSolve function.
## As per the assignment requirements it assumes that the supplied matrix is a square, invertible matrix so no checks or conversions are performed.
## Once the list is created the user can: 
## Use the get() and set() functions to read or replace the matrix.
## Use the getinverse() and setinverse() functions to read or replace the cached inverse matrix.
##
## example use:
## mymatrix<-makeCacheMatrix(matrix(rexp(16,rate=.1),ncol=4))
## cacheSolve(mymatrix)
## 
## if mymatrix is invertible then mymatrix$get() %*% cacheSolve(mymatrix) must return an identity matrix:
##
## mymatrix$get()%*%cacheSolve(mymatrix)
##              [,1]         [,2]          [,3]          [,4]
## [1,]  1.000000e+00 2.368982e-17  3.068292e-17 -2.523481e-17
## [2,] -1.046255e-16 1.000000e+00  5.410169e-17  1.016440e-17
## [3,] -7.047314e-19 1.902775e-17  1.000000e+00  1.032703e-17
## [4,] -2.439455e-17 8.977194e-17 -6.689527e-17  1.000000e+00
## please note that the numbers outside the diagonal are almost zero - ten to the minus 16 or smaller
makeCacheMatrix <- function(x = matrix()) {
  invc <- NULL
  set <- function(y) {
    x <<- y
    invc <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invc <<- inverse
  getinverse <- function() invc
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix that was created with the makeCacheMatrix function.
## If the inverse is available in the cache it returns the cached copy.
## If the inverse is not available in the cache it calculates, caches and then returns it.
cacheSolve <- function(x, ...) {
  tmp <- x$getinverse()
  if(!is.null(tmp)) {
    message("getting cached data")
    return(tmp)
  }
  data <- x$get()
  tmp <- solve(data)
  x$setinverse(tmp)
  tmp
}
