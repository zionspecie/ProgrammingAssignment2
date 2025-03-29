## Put comments here that give an overall description of what your
## functions do
## Two functions here to get the inverse of a matrix if it exists already, or
##Set the inverse if it doesn't exist already

## Write a short comment describing this function
## This first function gets a matrix and initializes the inverse as null and 
## Also has functions to set the inverse and get the inverse, both of which will be
## used in the second function
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solv) inv <<- solv
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This second function calls the getinverse function from the first function. 
##If the value is NULL, which is the default set there, then it will run the
##Solve() function to get the inverse and then run the setinverse function in
##function to cache the result
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
