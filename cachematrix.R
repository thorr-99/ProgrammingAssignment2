## The two functions here are combined to calculate or retrieve the inversion of a matrix.
## makeCacheMatrix will initiate an object with an input matrix. 
## cacheSolve will take the `makeCacheMatrix`` object and return an inverse matrix.  `

## This function will take a matrix as input to create an object with four methods. 

makeCacheMatrix <- function(x = matrix()) {
  inv <<- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invmat) inv <<- invmat
  getinv <- function() inv
  list(set = set, get = get, 
       setinv = setinv, getinv = getinv)
  
}


## cacheSolve retrive the inversed matrix from cache or, 
## calculate the inverse, save the result to cache and return it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("get cached inverse")
    return(inv)
  } 
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}
