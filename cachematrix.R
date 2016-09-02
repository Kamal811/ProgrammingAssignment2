## The function makeCacheMatrix creates an instance of the matrix and the inverse of the matrix in the memory 
## This facilitates that the inverse calculation need not be done everytime for the same object
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) 
  { 
    x <<- y  
    m <<- NULL
  }
  
  get <- function()  x
  
  setInvMatrix <- function(inverse)  m <<- inverse
  getInvMatrix <- function()  m 
  
  list(set = set , get=get, setInvMatrix = setInvMatrix , getInvMatrix = getInvMatrix)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInvMatrix()
  
  if(!is.null(inv))
  {
    message("Getting data from cache")
    return(inv)
  }
  
  srcMatrix <- x$get()
  inv <- solve(srcMatrix)
  x$setInvMatrix(inv)
  
  message("Setting data into cache")
  
  inv
}
