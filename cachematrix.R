## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  #set the m value to NULL 
  set <- function(y) { #set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x #get the matrix
  setinv <- function(solve) m <<- solve #set the inverse
  getinv <- function() m #get the inverse
   list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv() #obtain the inverse
  if(!is.null(m)) { #determine if inverse matrix was calculated 
    message("getting cached data")
    return(m)
  }
  data <- x$get()    #if inverse matrix was not calculated obtain the inverse value
  m <- solve(data, ...)
  x$setinv(m)
  m
}
