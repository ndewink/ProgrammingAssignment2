## Put comments here that give an overall description of what your
## functions do
## These funcations allow r to see if a value matrix inverse has already been canculated in the global environment. 
##If a matrix's inverse has already been caluclated the values are returned from cache, if they have not the inverse is calculated

## Write a short comment describing this function
## The makeCacheMatrix function generates and empty matrix and evaluates the inverse of the 
## Matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve ## sets the function to solve globally
  getInverse <- function() m ##retrieves the results of the function
  list(set = set, get = get,   ## generates a list of evaluated functions, so cache solve can determine if results are already created.
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## Evaluates if a matrix inverse has been calucated for x, if so it returns cached value, if not it calculates the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInvsere()
  if(!is.null(m)) { ##Checks to see if the inverse is already stored in cache
    message("getting cached data")
    return(m) ##if found in cache return the cached inverse
  }
  data <- x$get()
  m <- solve(data, ...) ## If m is not set by a value found in Cache, solve the data (calulcate the inverse)
  x$setInverse(m) 
  m ## return the inverse
}
  
