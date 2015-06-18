## Here are two functions that are used to create a special object that stores 
## a matrix and caches its inverse.

## makeCachedMatrix creates a special "matrix", which is really a list 
## containing a function to
## 1- set the value of the matrix
## 2- get the value of the matrix
## 3- set the value of the inverse
## 4- get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "matrix" created using
## makeCacheMatrix, but first it checks to see if the inverse has already been
## calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
