## This R script contains 2 functions
##
## 1. makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
## Sample usage:
## y <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
## 
## 2. cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cacheSolve should 
## retrieve the inverse from the cache.
## Sample usage:
## cacheSolve(y) ## first call will have to compute the inverse of the matrix, 2nd call onwards will get cached value.

## function: makeCacheMatrix
## input - x: an invertible matrix
## returns a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set inverse of the matrix
## 4. get inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL  ## this ensures that if the matrix changes, cacheSolve()
                ## function will recompute and not use cached value
  }
  get <- function() x
  setInvMatrix <- function(solve) m <<- solve
  getInvMatrix <- function() m
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}

## function: cacheSolve
## calculates the inverse of the special "matrix" created with function: makeCacheMatrix(). 
## However, it first checks to see if the inverse of the has already been calculated. If so, 
## it gets the inverse of the matrix from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the matrix and sets the value of the inverse in the cache via 
## the setInvMatrix function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    m <- x$getInvMatrix()
    if(!is.null(m)) {
      message("getting cached inverted matrix")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInvMatrix(m)
    m
}
