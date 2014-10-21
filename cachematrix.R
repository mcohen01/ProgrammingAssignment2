## Matrix inversion is usually a costly computation. 
## These functions together cache the inverse of a 
## matrix rather than computing it repeatedly.

## minimal "caching" api using R's lexical scoping rules
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse <<- inverse  
  getinverse <- function() inverse
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## compute the inverse of the matrix if not already computed
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

## test code as specified in https://class.coursera.org/rprog-008/forum/thread?thread_id=174
# amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
# amatrix$get() 
# cacheSolve(amatrix) 
# amatrix$getinverse() 
# amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) 
# cacheSolve(amatrix)
# amatrix$get()
# amatrix$getinverse()