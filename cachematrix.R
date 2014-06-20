## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x=matrix()) {
  # m will store the cached inverse matrix
  m <- NULL
  # Setter function for the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Getter function for the matrix
  get <- function() x
  # Setter function for the inverse
  setInverse <- function(inverse) m <<-inverse
  # Setter function for the inverse
  getInverse <- function() m
  # Return the newly defined functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Computes the inverse of the matrix returned
  ## by makeCacheMatrix(), unless the inverse has
  ## already been calculated, in which case
  ## it retrieves it from the cache.
  m <- x$getInverse()
  if ( ! is.null(m)) {
    print("getting cached data")
    return(m)
  }
  m <- solve(x$get())
  x$setInverse(m)
  m
}
