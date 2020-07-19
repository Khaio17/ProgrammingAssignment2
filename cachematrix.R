## Put comments here that give an overall description of what your
## functions do
# My functions create a "matrix" object that stores its and its inverse as well as computes its inverse
## Write a short comment describing this function
# create a "matrix object" that stores its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y){ 
    x <<- y  
    inv <<- NULL
  } 
  get <- function() x 
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse )
}


## Write a short comment describing this function
# computes the reverse of the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
