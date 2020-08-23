##makeCacheMatrix will create an special object that can cache its inverse. 
##cacheSolve will compute the inverse matrix from the previous result.
#If the inverse has already been been calculated, then it will retrieve the inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
  i <- NULL ##Value of the inverse of the matrix
  set <- function(y) { ##Assigning a new variable with function(y)
    x <<- y ##Value of Matrix
    i <<- NULL ##If it's a new matrix, reset the value to NULL
  }
  get <- function() x #New variable with function(x)
  setinverse <- function(inverse) i <<- inverse #Assigns a new variable with function(inverse)
  getinverse <- function() i ##Gets the inverse 
  list(set = set, ##Need this to run de function within the list
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ##Returns a matrix that is the inverse of "x"
  i <- x$getinverse() ##Variable that gets the inverse
  if (!is.null(i)) { ##Checks if the inverse has already been calculated
    message("getting cached data")
    return(i)
  }
  data <- x$get() ##Solves the inverse
  i <- solve(data, ...)
  x$setinverse(i)
  i
}