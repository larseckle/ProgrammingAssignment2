## Put comments here that give an overall description of what your
## functions do

# able to cache potentially time-consuming computations. For example, 
# in this case calculatin the inverse of a matrix
# If the contents of a matrix does not change, it may make sense to 
# cache the value of the inverse so that when we need it again
# it canbe read instead of newly calculated
# it is nice example how to use lexical scoping in R


# this first function makeCacheMatrix creates a "matrix", which is really a list
# containing a functions to
# it takes a datatype matrix as an initial argument
# set the contents of a matrix
# get the contents of a matrix
# set the value of the inverted matrix
# get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  # initializing the inverse
  i <- NULL
  #initialize the matrix when calling set, but not calcuating the inverse yet
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # simply returning the matrix 
  get <- function() x
  
  # setting the inverse of the matrix 
  setinverse <- function(inverse) i <<- inverse
  
  # setting the inverse of the matrix 
  getinverse <- function() i
  
  # returning the list of functions 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


# this function cacheSolve calculates the inverse of the matrix which was 
# stored with above function
# it checks first whether the inverse was calculated already and
# in case skips the calculation.
# otherwise it calculates the inversion, stores it and returns it


cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
  
}