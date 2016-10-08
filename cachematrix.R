## https://github.com/nmnbbdt/ProgrammingAssignment2.git
## 1st commit SHA-1 hash identifier: fff5ab986118685eeaef5ac0500cc71d76ef4798
## R Programming Assignment 2: Lexical Scoping--caching the inverse of a matrix

## The makeCacheMatrix function creates a special "matrix",
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  sI <- function(inverse) inv <<- inverse
  gI <- function() inv
  list(set = set,
       get = get,
       sI = sI,
       gI = gI)
}


## The cacheSolve function calculates the inverse of the special "matrix"
## the special "matrix" which created with the makeCacheMatrix function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$gI()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$sI(inv)
  inv
}

