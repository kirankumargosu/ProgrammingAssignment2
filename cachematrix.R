
## This function makes the Cache `Matrix.
## It has a setter, 
##        a getter
##        a function to setInverse
##        a function to getInverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function () {
    x
  }
  
  setInverse <- function (x) {
    inv <<- x
  }
  
  getInverse <- function () {
    inv
  }
  
  list (set = set, 
        get = get, 
        setInverse = setInverse, 
        getInverse = getInverse)
}



## This funtion invokes the makeMatrix functions
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## 1. Get Inverse from cache
  mInv <- x$getInverse()
  
  # 2. If Inverse is not null (already computed), return Inverse
  if(!is.null(mInv)) {
    message("getting cached inverse matrix")
    return(mInv)
  }
  
  # 3. Get Matrix from Cache and compute Inverse, if Matrix is square
  dataMatrix <- x$get()
  if(dim(dataMatrix)[1] != dim(dataMatrix)[2]) {
    message("the argument must be a square matrix")
    return(NULL)
  }
  
  mInv <- solve(dataMatrix, ...)
  
  # 4. Store Inverse in cache
  x$setInverse(mInv)
  
  # 5. Return Inverse
  mInv
}
