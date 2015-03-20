## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The first function, makeCacheMatrix creates a special matrix, which is really a list of matrix contain functions that
# set the value of the matrix by set(matrix)
# get the value of the matrix by get()
# set the value of the inverse matrix by setsolve(inverse matrix)
# get the value of the inverse matrix by getsolve()


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
# The following function calculates the inverse of the special matrix created with the above function.
# It first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse matrix in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

