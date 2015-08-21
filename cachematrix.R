##makeCacheMatrix will create the special object that can be used by cacheSolve
##cacheSolve will verify if the inverse is already cached and return it
##if it is not cached, it will calculate it and cache it for next calls
##
## 

##create the special object that has 4 functions
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<-y
    inv <<-NULL
  }
  get <- function() x
  setinv <- function (inverseMatrix) inv <<-inverseMatrix
  getinv <- function() inv
  list (set = set, get = get, setinv = setinv, getinv=getinv)
}


## calculate and cache the inverse if it is not already cached
##return the cahced if it is present
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)){
    message("using cached matrix")
    return (inv)
  }
  inv <- solve(x$get(),...)
  x$setinv(inv)
  inv
}
