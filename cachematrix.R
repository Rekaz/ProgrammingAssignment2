##makeCacheMatrix function allows us to do the following things:-
##1. set the value of matrix
##2. get the value of the matrix
##3. set the value of the inverse
##4. get the value of the inverse
##All the above mentioned things are then put in a list

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
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


##cacheSolve function returns the inverse of the matrix that is created by the above function
##We have used the function solve() to calculate the inverse of the matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv<- solve(data,...)
  x$setinverse(inv)
  inv
}