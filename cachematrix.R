## Below are two functions that are used to create a special object 
## that stores an invertible matrix and cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## - set the value of the matrix,
## - get the value of the matrix,
## - set the value of the inverse,
## - get the value of the inverse.

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(new_mat) {
    mat <<- new_mat
    inv <<- NULL
  }
  get <- function() mat
  setinverse <- function(calc_inv) inv <<- calc_inv
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via
## the setinverse function.

cacheSolve <- function(c_mat, ...) {
  inv <- c_mat$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- c_mat$get()
  inv <- solve(data, ...)
  c_mat$setinverse(inv)
  inv
}

## Test
mat <- as.matrix(cbind(c(1, 0, 0), c(0, 5, 0), c(0, 0, 10))) # Invertible test matrix
c_mat <- makeCacheMatrix(mat) # create the cache matrix object
cacheSolve(c_mat) # Actually calculates the inverse
cacheSolve(c_mat) # Returns the cached value (displays the message "getting cached data") 
