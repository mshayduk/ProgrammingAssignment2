  ## This script provides functions to construct and access the special square matrix object
  ## able to cache it's inverse to avoid potentially time-consuming inverse re-calculations 

  # obj <- makeCacheMatrix(x = matrix())  constructs object from square matrix x that can hold x's inverse
  # list of methods: set(matrix), get(), setinverse(matrix), getinverse()
  makeCacheMatrix <- function(x = matrix()) {
    if (!is.matrix(x) | (is.matrix(x) & (dim(x)[1]!=dim(x)[2])))
      stop("argument is not a square matrix")
    
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverseOfx) inv <<- inverseOfx
    getinverse <- function() inv
    list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
  }

  # cacheSolve(x,...)  returns the inverse of the square matrix in object x
  # it returns cached inverse if it is present in x 
  cacheSolve <- function(x, ...) {
    # trying to get the cached inverse of the data in x
    inv <- x$getinverse()
    # if it exsists/NOT NULL, returning it, avoiding re-calculation
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    # if the cached inverse is not existing, calculating and caching it in x
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
