makeCacheMatrix <- function(mat = matrix() {
  
  #instantiate an object to store the inverse
  inverse <- NULL
  
  #function to set a new value to the matrix
  set <- function(y) {
    mat <<- y
    inverse <<- NULL
  }
  
  #function to get current value of the matrix
  get <- function() mat
  
  #function to set the inverse of the matrix
  setinverse <- function(inv) inverse <<- inv
  
  #function to get the inverse of the matrix
  getinverse <- function() inverse
  
  #join all functions to a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  
  #get current value of inverse
  inv <- x$getinverse()
  
  #if inverse is cached, return cached data
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #if we are here, inverse is not cached
  
  #get value of matrix
  mat <- x$get()
  
  #calculate the inverse using solve()
  inv <- solve(mat, ...)
  
  #set inverse calculated to be cached
  x$setinverse(inv)
  
  #return the inverse
  inv
}
