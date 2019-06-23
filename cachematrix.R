## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# create a matrix object that cache it's inverse
makeCacheMatrix <- function(m = matrix()){
  
  #initial inverse
  m <- NULL
  
  # set matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #get matrix
  get <- function()x
  
  #inverse
  setIn <- function(Inverse) m <<- Inverse
  getIn <- function() m
  
  list(set = set, get = get,
       setIn = setIn ,
       getIn = getIn )
  
  
}

##This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated
#(and the matrix has not changed), then cacheSolve should retrieve 
#the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getIn()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- matrix.inverse(data)
  
  x$setIn(m)
  
  m
}