## Assignment: Caching the Inverse of a Matrix
## Write the following functions:
##  makeCacheMatrix: This function creates a special "matrix" object 
##     that can cache its inverse.
##  cacheSolve: This function computes the inverse of the special 
##    "matrix" returned by makeCacheMatrix 
##     If the inverse has already been calculated (and the matrix has not changed), 
##     then the cachesolve should retrieve the inverse from the cache.

## Input: a matrix
## Returns list of four functions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) cachedInverse <<- solve
  getInverse <- function() cachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Input: matrix wrapped by the makeCacheMatrix function
## Returns a matrix that is the inverse of 'x'
## Assumes that the matrix is invertible
## memoized solve() for matrices returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  cachedInverse <- x$getInverse()
  if(!is.null(cachedInverse)) {
    message("getting cached data")
    return(cachedInverse)
  }
  data <- x$get()
  cachedInverse <- solve(data, ...)
  x$setInverse(cachedInverse)
  cachedInverse
}



## test matrix
solveableMatrix <- makeCacheMatrix(matrix(c(1.1,2.2,3.3,-4.4), nrow=2, ncol=2))
#cacheSolve(nonSquareMatrix)
##round tripping
#cacheSolve(makeCacheMatrix(cacheSolve(solveableMatrix)))

