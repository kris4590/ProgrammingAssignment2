## For solving this assignment we use two functions 'makeCacheMatrix' and 'cacheSolve'.
## The makeCacheMatrix function creates a matrix object capable to cache its inverse.
## The cacheSolve function computes the inverse of the matrix obtained from the makeCacheMatrix function.If the inverse already exsists ,it retrieves the matrix from the cache.

## We assume that the matrix supplied is always invertible.

##The makeCacheMatrix function creates a matrix, which is really a list containing a function to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
    k <- NULL
  set <- function(y) {
          x <<- y
          k <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) k <<- inverse
  getinverse <- function() k
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



##  The cacheSolve function computes the inverse of the matrix obtained from the makeCacheMatrix function.If the inverse already exsists ,it retrieves the matrix from the cache.

cacheSolve <- function(x, ...) {
         k <- x$getinverse()
  if (!is.null(k)) {
          message("getting cached data")
          return(k)
  }
  data <- x$get()
  k <- solve(data, ...)
  x$setinverse(k)
  k
}
