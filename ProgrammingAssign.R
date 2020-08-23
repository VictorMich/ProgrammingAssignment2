##makeCacheMatrix will create an special object that can cache its inverse. 
##cacheSolve will compute the inverse matrix from the previous result.
#If the inverse has already been been calculated, then it will retrieve the inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
  i <- NULL 
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ##Returns a matrix that is the inverse of "x"
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}