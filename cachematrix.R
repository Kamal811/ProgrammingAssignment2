## Put comments here that give an overall description of what your
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
