## Put comments here that give an overall description of what your
## functions do
## Hi, I hope this find you well. There are my 2 functions that I create using the 2 examples functions makevector and cachemean


## Write a short comment describing this function
## This function create a vector of 4 functions that will be used by the function n*2 to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function looks if the inverse of the matrix was already calculated, 
## Then do the calcul of the inverse if that is not the case 
## and then return the value of the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
