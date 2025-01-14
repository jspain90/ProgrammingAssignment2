
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse

    getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  inv <- x$getInverse()

  data <- x$get()
  
  inv <- solve(data, ...)
  
  x$setInverse(inv)
  
  inv
}


# x <- matrix(1:4, 2, 2)
# cacheMatrix <- makeCacheMatrix(x)
# inv <- cacheSolve(cacheMatrix)
# inv