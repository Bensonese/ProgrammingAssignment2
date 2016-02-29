## The makeCacheMatrix function stores an n-by-n (square) matrix in memory
## as well as caches the inverse of this particular matrix if available.
## The cacheSolve function computes and stores the inverse of the
## square matrix returned by makeCacheMatrix if none is already available
## in the memory.

## makeCacheMatrix creates a special "square matrix" object that can
## cache its inverse and rejects all non-valid inputs such as
## non-matrices or non-square matrices

makeCacheMatrix <- function(x = matrix()) {
     
     if(!is.matrix(x)) {
          stop("This function can only take a matrix as its argument!")
     } else if (nrow(x) != ncol(x)) {
          stop("Sorry, this function can only accept a SQUARE matrix.")
     }
     
     inv <- NULL
     
     set <- function(y) {
          
          if(!is.matrix(y)) {
               stop("This function can only take a matrix as its argument!")
          } else if (nrow(y) != ncol(y)) {
               stop("Sorry, this function can only accept a SQUARE matrix.")
          }
          
          x <<- y
          
          inv <<- NULL
          
     }
     
     get <- function() x
     
     setinv <- function(b) {
          
          if(!is.matrix(b)) {
               stop("This function can only take a matrix as its argument!")
          } else if (nrow(b) != ncol(b)) {
               stop("Sorry, this function can only accept a SQUARE matrix.")
          }
          
          inv <<- b
          
     }
     
     getinv <- function() inv
     
     list(set = set, get = get, setinv = setinv, getinv = getinv)
     
}

## cacheSolve computes the inverse of the square matrix returned by
## makeCacheMatrix if none is already available from the cache

cacheSolve <- function(x, ...) {
     
     inv <- x$getinv()
     
     if(!is.null(inv)) {
          message("Cached data retrieved")
          return(inv)
     }
     
     b <- x$get()
     
     inv <- solve(b, ...)
     
     x$setinv(inv)
     
     message("New matrix inverse calculated")
     
     inv
     
}