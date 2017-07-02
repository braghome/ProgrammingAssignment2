print(R.version.string)
library(corpcor)
## the makeCacheMatrix builds a list of getter and setters for matrix and inverse 
## cacheSolve populats the cache of the inverse(also handeles non-square matrix inverse) of the matrix pass on from the list of getters and setters

## the function will build a list of functions that will maintain inverse and the matrix 
makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverseMatrix = matrix()) inver <<- inverseMatrix
  getinverse <- function() inver
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## should return an inverse(also handeles non-square matrix inverse) of a matrix and might even use the inverse from cache if present
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv) && !all(is.na(inv))) {
    message("getting cached data")
    return(inv)
  }
  dataM <- x$get()
  inv <- if(isSymmetric(dataM)) {
    solve(dataM, ...)
  } else {
    pseudoinverse(dataM)
  }
  x$setinverse(inv)
  inv
}
