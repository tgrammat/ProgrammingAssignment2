## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The constructor function makeCacheMatrix creates a "special" matrix the inverse of which can be cached.

## This is possible through the inner function calls with which:
## 1. one can set the value of a Matrix
## 2. one can get the value of a Matrix
## 3. one can set the value of its Inverse Matrix
## 4. one can get the value of its Inverse Matrix

## The <<- operator assign values in the environment of each function that this "special", cached Matrix  
## will be eventually called.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## This function calculate the inverse of a cached matrix X, that is constructed with the makeCacheMatrix() function.

## More specifically, and by utilizing the makeCacheMatrix() calls:
## 1. Retrieve the inverse of X if this matrix has been already cached.
## 2. Retrieve the original cached matrix X if no inverse matrix has been cached.
## 3. Compute the inverse of X by calling solve(Matrix,...)
## 4. Store this inverse matrix as a setinv() object of this "special" cached Matrix X

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  Matrix <- x$get()
  m <- solve(Matrix, ...)
  x$setinv(m)
  m
}
