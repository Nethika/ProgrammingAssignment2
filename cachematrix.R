## These are two functions that are used to create a special object 
## that stores a matrix and cache's its inverse.

## makeCacheMatrix creates a special "matrix".It is a list containing functions tp 
## set a matrix, get the matrix,set the Inverse and to get the inverse

makeCacheMatrix <- function(x = matrix()) {
I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) I <<- Inverse
  getInverse <- function() I
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function calculates the inverse of the special "matrix" 
## created with the above function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   I <- x$getInverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setInverse(I)
  I       
}
