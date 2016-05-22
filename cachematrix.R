## Creates a special matrix object that can cache its inverse
## Matrix object with attributes get, set, getinverse and setinverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  mObject <- list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse,
       localMatrix = NULL)

  # storing the matrix in the object
  mObject$set(x)
  # Get inverse of a matrix
  cacheSolve(mObject)
}


## cachesolve function will fetch inverse of a matrix from cache, if present, else will calculate the matrix inverse and store in cache

cacheSolve <- function(mCmObj, ...) {

  m <- mCmObj$getinverse()
  if(!is.null(m)) {
    # check if the inverse is found and the matrix has not changed
    if (mCmObj$localMatrix == mCmObj$get()) {
      message("Fetching inverse of a matrix from cache")
      return(m)
    }
  }
  # storing the matrix in localMatrix
  data <- mCmObj$get()

  # storing the matrix to compare, in the next function calls,
  # to check whether the matrix value has changed before fetching the cached inversed matrix
  mCmObj$localMatrix = data

  # calculate the inverse of a matrix
  matrixInverse <- solve(data, ...)
  # store matrix inverse
  mCmObj$setinverse(matrixInverse)
  ## Return a matrix that is the inverse of 'mCmObj'
  matrixInverse
}
